//! Support for bevy
use crate::utils::renderer::{Printable, Renderer};

use crate::{Tick, OnTick};
use crate::Typeable;
use crate::{DrawTime, NumLike};
use crate::style::{self, DefaultStyle};
use bevy::{
    ecs::{
        component,
        system::{SystemMeta, SystemParam},
        world::unsafe_world_cell::UnsafeWorldCell,
    },
    input::keyboard::KeyboardInput,
    utils::Duration,
};
use promise_out::{
    pair::{Consumer, Producer},
    Promise,
};
use std::borrow::Cow;
use std::sync::{Arc, Mutex};
// use std::rc::Rc;

use bevy::prelude::*;
use std::future::Future;
use std::fmt::Debug;
use futures_lite::future;

use std::ops::{Deref, DerefMut};

use crate::text_style_adapter::StyledStringWriter;
use crate::{Confirm, Error, Message, MultiSelect, Number, Password, Select, Toggle, Valuable};
use bevy::tasks::{block_on, AsyncComputeTaskPool, Task};
use bevy::window::RequestRedraw;
use itertools::Itertools;
use text_style::{bevy::TextStyleParams, AnsiColor, StyledString};
use bevy_defer::{AsyncCommandsExtension, AsyncExecutor, AsyncPlugin, world, AsyncAccess, access::AsyncQuery};

/// The Asky prompt
///
/// TODO: Consider renaming to AskyPrompt.
#[derive(Component, Debug)]
pub struct AskyNode<T: Typeable<KeyEvent> + Valuable> {
    prompt: T,
    promise: Option<Producer<T::Output, Error>>,
}

impl<T: Typeable<KeyEvent> + Valuable> AskyNode<T> {
    /// Create a new AskyNode without a promise.
    pub fn new(prompt: T) -> Self {
        AskyNode {
            prompt,
            promise: None
        }
    }
}

/// A delay
#[derive(Component, Debug)]
pub struct AskyDelay(Timer, Option<Producer<(), Error>>);

/// The local state of an Asky prompt.
///
/// TODO: Rename to AskyPromptState?
#[derive(Debug, Default, Component, Reflect)]
pub enum AskyState {
    #[default]
    /// Waiting for input
    Waiting,
    /// Finished
    Complete,
    /// Not clear this does anything.
    Hidden,
}

/// The global State of the asky prompt.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, States, Reflect)]
pub enum AskyPrompt {
    /// No propmt is active.
    #[default]
    Inactive,
    /// A prompt is active.
    Active,
}

fn run_timers(mut commands: Commands, mut query: Query<(Entity, &mut AskyDelay)>, time: Res<Time>,
    mut redraw: EventWriter<RequestRedraw>,
) {
    for (id, mut asky_delay) in query.iter_mut() {
        asky_delay.0.tick(time.delta());
        if asky_delay.0.finished() {
            asky_delay.1.take().expect("Promise not there").resolve(());
            commands.entity(id).remove::<AskyDelay>();
        }
        // I would RequestTick to just run the systems once, but this seems to
        // be the way.
        redraw.send(RequestRedraw);
    }
}

/// The gateway to asky functionality in bevy.
///
/// This is a bevy [SystemParam] so any system can access it.
#[derive(Clone, SystemParam)]
pub struct Asky {
    // config: AskyParamConfig,
    // executor: AsyncExecutor,
}

/// Asky's global state.
#[derive(Resource, Clone)]
pub struct AskyParamConfig {
    pub(crate) state: Arc<Mutex<AskyParamState>>,
}

/// The closure type for asky
pub type Closure = dyn FnOnce(&mut Commands, Option<Entity>, Option<&Children>)
                              -> Result<(), Error> + 'static + Send + Sync;

/// Consider making a typedef.
pub struct AskyParamState {
    pub(crate) closures: Vec<(Box<Closure>, Option<Entity>)>,
}

impl Asky {
    /// Create a new Asky.
    // fn new(config: AskyParamConfig) -> Self {
    //     Self { config }
    // }

    /// Run a closure soon.
    // pub fn run<C>(&mut self,
    //            // closure: impl Closure,
    //            closure: C,
    //            entity: Option<Entity>) where
    // C: FnOnce(&mut Commands, Option<Entity>, Option<&Children>)
    //                                 -> Result<(), Error> + 'static + Send + Sync {
    //     // use bevy::ecs::system::RunSystemOnce;
    //     // let world = world();
    //     // world.run(move |world|
    //     //           {
    //     //           world.run_system_once_with((entity, closure), |In((entity, closure)): In<(Option<Entity>, C)>,
    //     //                                      mut commands: Commands, query: Query<Option<&Children>>| -> T {
    //     //               let children = entity.and_then(|id| query.get(id).expect("Unable to get children"));
    //     //               let result = closure(&mut commands, entity, children);
    //     //                                          result.unwrap()

    //     //           })
    //     //           })
    //     // self.config.state.lock().unwrap().closures.push((Box::new(closure), entity));
    // }

    /// Prompt the user with `T`, rendering in element `dest`.
    pub fn prompt<T: Typeable<KeyEvent> + Valuable + Send + Sync + 'static>(
        &mut self,
        prompt: T,
        dest: Entity,
    ) -> impl Future<Output = Result<T::Output, Error>> {
        async move {
            let (promise, waiter) = Producer::<T::Output, Error>::new();
            let world = world();
            let node = NodeBundle {
                style: Style {
                    flex_direction: FlexDirection::Column,
                    ..default()
                },
                ..default()
            };
            let id = world
                .spawn_bundle((node,
                               AskyNode { prompt, promise: Some(promise) },
                               AskyState::Waiting))
                .await
                .id();

            world.entity(dest).add_child(id).await;
            waiter.await
        }
    }

    /// Prompt the user with `T`, rendering in element `dest` with a given style.
    pub fn prompt_styled<T: Typeable<KeyEvent> + Valuable + Send + Sync + 'static>(
        &mut self,
        prompt: T,
        dest: Entity,
        style: AskyStyle
    ) -> impl Future<Output = Result<T::Output, Error>> {
        async move {
            let (promise, waiter) = Producer::<T::Output, Error>::new();
            let world = world();
            let node = NodeBundle {
                style: Style {
                    flex_direction: FlexDirection::Column,
                    ..default()
                },
                ..default()
            };
            let id = world
                .spawn_bundle((node,
                               AskyNode { prompt, promise: Some(promise) },
                               AskyState::Waiting,
                               style))
                .await
                .id();
            world.entity(dest).add_child(id).await;
            waiter.await
        }
    }

    /// Clear all entities in `dest`.
    pub fn clear(&mut self, dest: Entity) -> impl Future<Output = ()> {
        let world = world();
        world.entity(dest).despawn_descendants()
    }

    /// Delay for the duration.
    pub fn delay(&mut self, duration: Duration) -> impl Future<Output = ()> {
        let world = world();
        world.sleep(duration)
    }
}


/// Check components to determine whether a AskyPrompt state needs to change.
fn check_prompt_state(
    query: Query<&AskyState>,
    delays: Query<&AskyDelay>,
    asky_prompt: Res<State<AskyPrompt>>,
    mut next_asky_prompt: ResMut<NextState<AskyPrompt>>,
    mut redraw: EventWriter<RequestRedraw>,
) {
    let was_active = matches!(**asky_prompt, AskyPrompt::Active);
    let is_active = query.iter().filter(|x| matches!(*x, AskyState::Waiting)).next().is_some()
        || delays.iter().next().is_some();
    if was_active ^ is_active {
        next_asky_prompt.set(if is_active { AskyPrompt::Active } else { AskyPrompt::Inactive });
        redraw.send(RequestRedraw);
    }
}

// unsafe impl SystemParam for Asky {
//     type State = AskyParamConfig;
//     type Item<'w, 's> = Asky;

//     fn init_state(world: &mut World, _system_meta: &mut SystemMeta) -> Self::State {
//         world
//             .get_resource_mut::<AskyParamConfig>()
//             .expect("No AskyParamConfig setup.")
//             .clone()
//     }

//     #[inline]
//     unsafe fn get_param<'w, 's>(
//         state: &'s mut Self::State,
//         _system_meta: &SystemMeta,
//         _world: UnsafeWorldCell<'w>,
//         _change_tick: component::Tick,
//     ) -> Self::Item<'w, 's> {
//         // let exe = world
//         //     .get_non_send_resource_mut::<AsyncExecutor>()
//         //     .expect("No AskyParamConfig setup.")
//         //     .clone();
//         Asky {
//             // config: state.clone(),
//             // executor: exe
//         }
//         // Asky::new(state.clone())
//     }
// }

/// An asky task.
///
/// [Asky] is the source of these tasks. This is the sink for those tasks.
// #[derive(Component)]
// pub struct TaskSink<T>(pub Task<T>);

// impl<T: Send + 'static> TaskSink<T> {
//     /// Create a new [TaskSink] for the given future.
//     pub fn new(future: impl Future<Output = T> + Send + 'static) -> Self {
//         let thread_pool = AsyncComputeTaskPool::get();
//         let task = thread_pool.spawn(future);
//         Self(task)
//     }
// }

/// Given a future, create a [TaskSink] for it.
pub fn future_sink<T: 'static, F: Future<Output = T> + 'static>(
    In(future): In<F>,
    exec: NonSend<AsyncExecutor>,
    // mut commands: Commands,
) {
    exec.spawn(future);
    // commands.spawn_task(move || async {
    //     future.await;
    //     Ok(())
    // });
    // commands.spawn(TaskSink::new(future));
}

// pub fn future_result_sink<T: Send + 'static, F: Future<Output = Result<T, Error>> + Send + 'static>(
//     In(future): In<F>,
//     mut commands: Commands,
// ) {
//     commands.spawn(TaskSink::new(future));
// }

/// Given an optional future, only create a task sink if necessary.
pub fn option_future_sink<T: 'static, F: Future<Output = T> + 'static>(
    In(future_maybe): In<Option<F>>,
    exec: NonSend<AsyncExecutor>,
    // mut commands: Commands,
) {
    if let Some(future) = future_maybe {
        exec.spawn(future);
        // commands.spawn(TaskSink::new(future));
    }
}

/// Poll the [TaskSink]s.
// pub fn poll_tasks<T: Send + Sync + 'static>(
//     mut commands: Commands,
//     mut tasks: Query<(Entity, &mut TaskSink<T>)>,
// ) {
//     for (entity, mut task) in &mut tasks {
//         if block_on(future::poll_once(&mut task.0)).is_some() {
//             // Once
//             commands.entity(entity).despawn();
//         }
//     }
// }

/// Poll [TaskSink] which return `Result<T, E>`.
// pub fn poll_tasks_err<T: Send + Sync + 'static, E: Debug + Send + Sync + 'static>(
//     mut commands: Commands,
//     _asky: Asky,
//     mut tasks: Query<(Entity, &mut TaskSink<Result<T, E>>)>,
// ) {
//     for (entity, mut task) in &mut tasks {
//         if let Some(result) = block_on(future::poll_once(&mut task.0)) {
//             // Once
//             if let Err(error) = result {
//                 eprintln!("Got error here {:?}.", error);
//                 // FIXME: I need the right entity to make this work.
//                 // let _ = asky.prompt(Message::new(format!("{:?}", error)), entity);
//                 commands.entity(entity).despawn();
//             } else {
//                 commands.entity(entity).despawn();
//             }
//         }
//     }
// }

impl<T: Typeable<KeyEvent> + Valuable> Deref for AskyNode<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.prompt
    }
}

impl<T: Typeable<KeyEvent> + Valuable> DerefMut for AskyNode<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.prompt
    }
}

/// Represent the key events by their chars and their key codes.
pub struct KeyEvent {
    /// Characters from event
    pub chars: Vec<char>,
    /// [bevy::prelude::KeyCode] from event
    pub codes: Vec<KeyCode>,
}

impl KeyEvent {
    /// Return true if there are no chars or key codes.
    pub fn is_empty(&self) -> bool {
        self.chars.is_empty() && self.codes.is_empty()
    }
}

impl<T: Typeable<KeyCode>> Typeable<KeyEvent> for T {
    fn handle_key(&mut self, key: &KeyEvent) -> bool {
        let mut result = false;
        for code in &key.codes {
            result |= self.handle_key(code);
        }
        result
    }

    fn will_handle_key(&self, key: &KeyEvent) -> bool {
        for code in &key.codes {
            if self.will_handle_key(code) {
                return true;
            }
        }
        false
    }
}

impl KeyEvent {
    /// Create a [KeyEvent] from event readers.
    pub fn new(
        mut char_evr: EventReader<ReceivedCharacter>,
        mut key_evr: EventReader<KeyboardInput>,
    ) -> Self {
        Self {
            chars: char_evr.read().flat_map(|e| e.char.chars()).collect(),
            codes: key_evr
                .read()
                .filter_map(|e| {
                    if e.state == bevy::input::ButtonState::Pressed {
                        Some(e.key_code)
                    } else {
                        None
                    }
                })
                .collect(),
        }
    }
}

/// The principle system, handles a particular type of asky prompt.
pub fn asky_system<T>(
    mut commands: Commands,
    char_evr: EventReader<ReceivedCharacter>,
    key_evr: EventReader<KeyboardInput>,
    global_asky_style: Option<Res<AskyStyle>>,
    mut renderer: Local<StyledStringWriter>,
    mut query: Query<(Entity, &mut AskyNode<T>, &mut AskyState, Option<&Children>, Option<&AskyStyle>)>,
) where
    T: Printable + Typeable<KeyEvent> + Valuable + Tick + Send + Sync + 'static,
{
    let key_event = KeyEvent::new(char_evr, key_evr);
    for (entity, mut node, mut state, children, style_maybe) in query.iter_mut() {
        match *state {
            AskyState::Complete => {
                continue;
            }
            AskyState::Hidden => {
                if let Some(children) = children {
                    commands.entity(entity).remove_children(children);
                    for child in children {
                        commands.entity(*child).despawn_recursive();
                    }
                }
            }
            AskyState::Waiting => {
                let on_tick = node.tick();
                if !is_abort_key(&key_event)
                    && !node.will_handle_key(&key_event)
                    && renderer.state.draw_time != DrawTime::First
                    && matches!(on_tick, OnTick::Continue)
                {
                    continue;
                }
                // Make sure we always render it once before dealing with aborting.
                if renderer.state.draw_time != DrawTime::First {
                    // For the terminal it had an abort key handling happen here.
                    if is_abort_key(&key_event) || matches!(on_tick, OnTick::Abort) {
                        *state = AskyState::Complete;

                        let waiting_maybe = node.promise.take();//std::mem::replace(&mut state, AskyState::Complete);
                        if let Some(promise) = waiting_maybe {
                            promise.reject(Error::Cancel);
                        }
                        renderer.state.draw_time = DrawTime::Last;
                    } else if node.handle_key(&key_event) || matches!(on_tick, OnTick::Finish) {
                        // It's done.
                        *state = AskyState::Complete;
                        let waiting_maybe = node.promise.take();//std::mem::replace(&mut state, AskyState::Complete);
                        // let waiting_maybe = std::mem::replace(&mut state, AskyState::Complete);
                        if let Some(promise) = waiting_maybe {
                            match node.prompt.value() {
                                Ok(v) => promise.resolve(v),
                                Err(e) => promise.reject(e),
                            }
                        }
                        renderer.state.draw_time = DrawTime::Last;
                    }
                    if let Some(children) = children {
                        commands.entity(entity).remove_children(children);
                        for child in children {
                            commands.entity(*child).despawn_recursive();
                        }
                    }
                }
                renderer.cursor_pos = None;
                renderer.cursor_pos_save = None;
                let mut text_style = None;
                match style_maybe {
                    Some(style) => {
                        text_style = style.text_style.as_ref();
                        let _ = node.draw_with_style(&mut *renderer, &*style.style);
                    }
                    None => {
                        match global_asky_style {
                            Some(ref style) => {
                                text_style = style.text_style.as_ref();
                                let _ = node.draw_with_style(&mut *renderer, &*style.style);
                            }
                            None => {
                                let _ = node.draw(&mut *renderer);
                            }
                        }
                    }
                }
                bevy_render(&mut commands, text_style, &mut renderer, entity);
                // This is just to affirm that we're not recreating the nodes unless we need to.
                let draw_time = renderer.draw_time();
                eprint!(".");
                if draw_time == DrawTime::First {
                    renderer.update_draw_time();
                } else if draw_time == DrawTime::Last {
                    renderer.clear();
                    let waiting_maybe = node.promise.take();
                    *state = AskyState::Complete;
                    // let waiting_maybe = std::mem::replace(&mut node.1, AskyState::Complete);
                    if let Some(promise) = waiting_maybe {
                        match node.prompt.value() {
                            Ok(v) => promise.resolve(v),
                            Err(e) => promise.reject(e),
                        }
                    }
                }
            }
        }
    }
}

fn bevy_render(
    commands: &mut Commands,
    text_style: Option<&TextStyle>,
    out: &mut StyledStringWriter,
    column: Entity,
) {
    // -> io::Result<()>
    let white = AnsiColor::White.dark();

    let strings = if out.state.cursor_visible {
        out.drain_with_styled_cursor(white)
    } else {
        std::mem::take(&mut out.strings)
    };

    commands.entity(column).with_children(|column| {
        let mut next_line_count: Option<usize> = None;
        let mut line_count: usize = 0;
        let lines = strings
            .into_iter()
            .flat_map(|mut s| {
                let mut a = vec![];
                let mut b = None;
                if s.s.contains('\n') {
                    let str = std::mem::take(&mut s.s);
                    a.extend(str.split_inclusive('\n').map(move |line| StyledString {
                        s: line.to_string(),
                        ..s.clone()
                    }));
                } else {
                    b = Some(s);
                }
                a.into_iter().chain(b)
            })
            .group_by(|x| {
                if let Some(x) = next_line_count.take() {
                    line_count = x;
                }
                if x.s.chars().last().map(|c| c == '\n').unwrap_or(false) {
                    next_line_count = Some(line_count + 1);
                }
                line_count
            });

        // let mut line_num = 0;
        for (_key, line) in &lines {
            let style: TextStyleParams = match text_style {
                Some(text_style) => text_style.clone().into(),
                None => TextStyle::default().into()
            };
            column
                .spawn(NodeBundle {
                    style: Style {
                        flex_direction: FlexDirection::Row,
                        ..default()
                    },
                    ..default()
                })
                .with_children(|parent| {
                    // if out.state.cursor_visible && line_num == out.state.cursor_pos[1] {
                    //     text_style::bevy::render_iter(
                    //         parent,
                    //         &style,
                    //         cursorify_iter(line, out.state.cursor_pos[0], white),
                    //     );
                    // } else {
                        text_style::bevy::render_iter(parent, &style, expand_tabs(line));
                    // }
                });
            // line_num += 1;
        }
    });
}

fn expand_tabs(line: impl Iterator<Item = StyledString>) -> impl Iterator<Item = StyledString> {
    let tabstop = 8;
    let mut count = 0;

    line.map(move |mut styled| {
        let mut replacements = Vec::new();
        for (i, c) in styled.s.char_indices() {
            if c == '\t' {
                let spaces = tabstop - count % tabstop;
                replacements.push((i, spaces));
                count += spaces;
            } else {
                count += 1;
            }
        }
        replacements.reverse();
        for (i, spaces) in replacements {
            styled.s.replace_range(i..i+1, &" ".repeat(spaces));
        }
        styled
    })
}

fn is_abort_key(key: &KeyEvent) -> bool {
    for code in &key.codes {
        if code == &KeyCode::Escape {
            return true;
        }
    }
    false
}

/// Defines the style locally as a component or globally as a resource.
#[derive(Component, Resource)]
pub struct AskyStyle {
    style: Box<dyn style::Style + 'static + Send + Sync>,
    /// Text style if any
    pub text_style: Option<TextStyle>
}

impl AskyStyle {
    /// Create a new [AskyStyle] from a style.
    pub fn new<S: style::Style + Send + Sync + 'static>(style: S) -> Self {
        Self {
            style: Box::new(style),
            text_style: None
        }
    }

    /// Add [TextStyle] to this [AskyStyle].
    pub fn with_text_style(mut self, text_style: TextStyle) -> Self {
        self.text_style = Some(text_style);
        self
    }
}

impl Default for AskyStyle {
    fn default() -> Self {
        AskyStyle::new(DefaultStyle::default())
    }
}

/// The Asky plugin for bevy
///
/// This will add [bevy_defer::AsyncPlugin] with default settings unless it has
/// already been added.
pub struct AskyPlugin;

impl Plugin for AskyPlugin {
    fn build(&self, app: &mut App) {
        if let Some(type_registry) = app.world.get_resource_mut::<AppTypeRegistry>() {
            let mut type_registry = type_registry.write();
            type_registry.register::<AskyState>();
            type_registry.register::<AskyPrompt>();
        }

        if !app.is_plugin_added::<AsyncPlugin>() {
            app.add_plugins(AsyncPlugin::default_settings());
        }
        app
            .insert_resource(AskyParamConfig {
                state: Arc::new(Mutex::new(AskyParamState {
                    closures: Vec::new(),
                })),
            })
            .init_state::<AskyPrompt>()

            .add_systems(Update, (asky_system::<Number<u8>>,
                                  asky_system::<Number<u16>>,
                                  asky_system::<Number<u32>>,
                                  asky_system::<Number<u64>>,
                                  asky_system::<Number<u128>>,
                                  asky_system::<Number<i8>>,
                                  asky_system::<Number<i16>>,
                                  asky_system::<Number<i32>>,
                                  asky_system::<Number<i64>>,
                                  asky_system::<Number<i128>>,
                                  asky_system::<Number<f32>>,
                                  asky_system::<Number<f64>>))
            .add_systems(Update, (asky_system::<Confirm>,
                                  asky_system::<Toggle>,
                                  asky_system::<crate::Text>,
                                  asky_system::<Select<'_, Cow<'static, str>>>,
                                  asky_system::<Select<'_, &'static str>>,
                                  asky_system::<Password>,
                                  asky_system::<Message>,
                                  asky_system::<MultiSelect<'static, &'static str>>,
                                  asky_system::<MultiSelect<'_, Cow<'static, str>>>).chain())
            // .add_systems(PostUpdate, poll_tasks::<()>)
            // .add_systems(PostUpdate, poll_tasks_err::<(), Error>)
            .add_systems(PostUpdate, check_prompt_state)
            // .add_systems(PostUpdate, run_closures)
            .add_systems(PostUpdate, run_timers);
    }
}

// Confirm
impl Typeable<KeyCode> for Confirm<'_> {
    fn will_handle_key(&self, key: &KeyCode) -> bool {
        match key {
            KeyCode::ArrowLeft | KeyCode::KeyH => true,
            KeyCode::ArrowRight | KeyCode::KeyL => true,
            KeyCode::KeyY => true,
            KeyCode::KeyN => true,
            KeyCode::Enter | KeyCode::Backspace => true,
            _ => false,
        }
    }

    fn handle_key(&mut self, key: &KeyCode) -> bool {
        let mut submit = false;

        match key {
            // update value
            KeyCode::ArrowLeft | KeyCode::KeyH => self.active = false,
            KeyCode::ArrowRight | KeyCode::KeyL => self.active = true,
            // update value and submit
            KeyCode::KeyY => submit = self.update_and_submit(true),
            KeyCode::KeyN => submit = self.update_and_submit(false),
            // submit current/initial value
            KeyCode::Enter | KeyCode::Backspace => submit = true,
            _ => (),
        }

        submit
    }
}

// MultiSelect

impl<T> Typeable<KeyCode> for MultiSelect<'_, T> {
    fn handle_key(&mut self, key: &KeyCode) -> bool {
        use crate::prompts::select::Direction;
        let mut submit = false;

        match key {
            // submit
            KeyCode::Enter | KeyCode::Backspace => submit = self.validate_to_submit(),
            KeyCode::Space => self.toggle_focused(),
            // update value
            KeyCode::ArrowUp | KeyCode::KeyK => self.input.move_cursor(Direction::Up),
            KeyCode::ArrowDown | KeyCode::KeyJ => self.input.move_cursor(Direction::Down),
            KeyCode::ArrowLeft | KeyCode::KeyH => self.input.move_cursor(Direction::Left),
            KeyCode::ArrowRight | KeyCode::KeyL => self.input.move_cursor(Direction::Right),
            _ => (),
        }

        submit
    }
}

// Number

impl<T: NumLike> Typeable<KeyEvent> for Number<'_, T> {
    fn will_handle_key(&self, key: &KeyEvent) -> bool {
        for c in key.chars.iter() {
            if !c.is_control() {
                return true;
            }
        }

        for code in &key.codes {
            if match code {
                // submit
                KeyCode::Enter => true,
                // remove delete
                KeyCode::Backspace => true,
                KeyCode::Delete => true,
                // move cursor
                KeyCode::ArrowLeft => true,
                KeyCode::ArrowRight => true,
                _ => false,
            } {
                return true;
            }
        }

        false
    }
    fn handle_key(&mut self, key: &KeyEvent) -> bool {
        use crate::prompts::text::Direction;
        let mut submit = false;

        for c in key.chars.iter() {
            if !c.is_control() {
                self.insert(*c);
            }
        }

        for code in &key.codes {
            match code {
                // submit
                KeyCode::Enter => submit = self.validate_to_submit(),
                // remove delete
                KeyCode::Backspace => self.input.backspace(),
                KeyCode::Delete => self.input.delete(),
                // move cursor
                KeyCode::ArrowLeft => self.input.move_cursor(Direction::Left),
                KeyCode::ArrowRight => self.input.move_cursor(Direction::Right),
                _ => (),
            };
        }

        submit
    }
}

// impl<T: NumLike> Printable for AskyNode<Number<'_, T>> {
//     fn draw<R: Renderer>(&self, renderer: &mut R) -> io::Result<()> {
//         let mut out = ColoredStrings::default();
//         let cursor = (self.formatter)(self, renderer.draw_time(), &mut out);
//         renderer.show_cursor()?;
//         renderer.set_cursor(cursor)?;
//         renderer.print(out)
//     }
// }

impl<'a, T: NumLike + 'a> Default for Number<'a, T> {
    fn default() -> Self {
        Self::new("")
    }
}

// Password

impl Typeable<KeyEvent> for Password<'_> {
    fn will_handle_key(&self, key: &KeyEvent) -> bool {
        for c in key.chars.iter() {
            if !c.is_control() {
                return true;
            }
        }

        for code in &key.codes {
            if match code {
                // submit
                KeyCode::Enter => true,
                // type
                // KeyCode::Char(c) => self.input.insert(c),
                // remove delete
                KeyCode::Backspace => true,
                KeyCode::Delete => true,
                // move cursor
                KeyCode::ArrowLeft => true,
                KeyCode::ArrowRight => true,
                _ => false,
            } {
                return true;
            }
        }

        false
    }
    fn handle_key(&mut self, key: &KeyEvent) -> bool {
        use crate::prompts::text::Direction;
        let mut submit = false;

        for c in key.chars.iter() {
            if !c.is_control() {
                self.input.insert(*c);
            }
        }

        for code in &key.codes {
            match code {
                // submit
                KeyCode::Enter => submit = self.validate_to_submit(),
                // remove delete
                KeyCode::Backspace => self.input.backspace(),
                KeyCode::Delete => self.input.delete(),
                // move cursor
                KeyCode::ArrowLeft => self.input.move_cursor(Direction::Left),
                KeyCode::ArrowRight => self.input.move_cursor(Direction::Right),
                _ => (),
            };
        }

        submit
    }
}

// impl Printable for crate::bevy::AskyNode<Password<'_>> {
//     fn draw<R: Renderer>(&self, renderer: &mut R) -> io::Result<()> {
//         let mut out = ColoredStrings::default();
//         let cursor = (self.formatter)(self, renderer.draw_time(), &mut out);
//         renderer.show_cursor()?;
//         renderer.set_cursor(cursor)?;
//         renderer.print(out)
//     }
// }

// Select

impl<T> Typeable<KeyCode> for Select<'_, T> {
    fn handle_key(&mut self, key: &KeyCode) -> bool {
        use crate::prompts::select::Direction;
        let mut submit = false;

        match key {
            // submit
            KeyCode::Enter | KeyCode::Backspace => submit = self.validate_to_submit(),
            // update value
            KeyCode::ArrowUp | KeyCode::KeyK => self.input.move_cursor(Direction::Up),
            KeyCode::ArrowDown | KeyCode::KeyJ => self.input.move_cursor(Direction::Down),
            KeyCode::ArrowLeft | KeyCode::KeyH => self.input.move_cursor(Direction::Left),
            KeyCode::ArrowRight | KeyCode::KeyL => self.input.move_cursor(Direction::Right),
            _ => (),
        }

        submit
    }
}

// impl<T: Send> Printable for crate::bevy::AskyNode<Select<'_, T>> {
//     fn draw<R: Renderer>(&self, renderer: &mut R) -> io::Result<()> {
//         let mut out = ColoredStrings::default();
//         (self.formatter)(self, renderer.draw_time(), &mut out);
//         renderer.print(out)
//     }
// }

impl Typeable<KeyEvent> for crate::Text<'_> {
    fn will_handle_key(&self, key: &KeyEvent) -> bool {
        for c in key.chars.iter() {
            if !c.is_control() {
                return true;
            }
        }

        for code in &key.codes {
            use KeyCode::*;
            match code {
                Enter | Backspace | Delete | ArrowLeft | ArrowRight => return true,
                _ => (),
            }
        }
        false
    }

    fn handle_key(&mut self, key: &KeyEvent) -> bool {
        use crate::prompts::text::Direction;
        let mut submit = false;

        for c in key.chars.iter() {
            if !c.is_control() {
                self.input.insert(*c);
            }
        }

        for code in &key.codes {
            match code {
                // submit
                KeyCode::Enter => submit = self.validate_to_submit(),
                // remove delete
                KeyCode::Backspace => self.input.backspace(),
                KeyCode::Delete => self.input.delete(),
                // move cursor
                KeyCode::ArrowLeft => self.input.move_cursor(Direction::Left),
                KeyCode::ArrowRight => self.input.move_cursor(Direction::Right),
                _ => (),
            };
        }

        submit
    }
}

// impl Printable for AskyNode<crate::Text<'_>> {
//     fn draw<R: Renderer>(&self, renderer: &mut R) -> io::Result<()> {
//         let mut out = ColoredStrings::default();
//         let cursor = (self.formatter)(self, renderer.draw_time(), &mut out);
//         renderer.show_cursor()?;
//         renderer.set_cursor(cursor)?;
//         renderer.print(out)
//     }
// }

impl Typeable<KeyCode> for Toggle<'_> {
    fn handle_key(&mut self, key: &KeyCode) -> bool {
        let mut submit = false;

        match key {
            // update value
            KeyCode::ArrowLeft | KeyCode::KeyH => self.active = false,
            KeyCode::ArrowRight | KeyCode::KeyL => self.active = true,
            // submit current/initial value
            KeyCode::Enter | KeyCode::Backspace => submit = true,
            _ => (),
        }

        submit
    }
}

// impl Printable for AskyNode<Toggle<'_>> {
//     fn draw<R: Renderer>(&self, renderer: &mut R) -> io::Result<()> {
//         let mut out = ColoredStrings::default();
//         (self.formatter)(self, renderer.draw_time(), &mut out);
//         renderer.print(out)
//     }
// }

impl Typeable<KeyCode> for Message<'_> {
    fn will_handle_key(&self, _key: &KeyCode) -> bool {
        true
    }

    fn handle_key(&mut self, _key: &KeyCode) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_scan() {
        let a = [1, 2, 3, 4];

        let mut iter = a.iter().scan(0, |state, &x| {
            // each iteration, we'll multiply the state by the element ...
            *state += 1;

            // ... and terminate if the state exceeds 6
            if *state % 2 == 0 {
                return None;
            }
            // ... else yield the negation of the state
            Some(-x)
        });

        assert_eq!(iter.next(), Some(-1));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next(), Some(-3));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.next(), None);
    }
}

// impl Printable for AskyNode<Message<'_>> {
//     fn draw<R: Renderer>(&self, renderer: &mut R) -> io::Result<()> {
//         let mut out = ColoredStrings::default();
//         (self.formatter)(self, renderer.draw_time(), &mut out);
//         renderer.hide_cursor()?;
//         renderer.print(out)
//     }
// }
