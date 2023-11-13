use asky::bevy::*;
use asky::Error;
use promise_out::{pair::Producer, Promise};
use bevy::tasks::{AsyncComputeTaskPool, Task, block_on};
use futures_lite::future;
use asky::Confirm;

use bevy::{prelude::*, window::PresentMode};

#[derive(Component)]
pub struct Handled;

#[derive(Component)]
struct OnComplete<T: Send>(Task<T>);

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                title: "Bevy Asky Example".into(),
                resolution: (600., 400.).into(),
                present_mode: PresentMode::AutoVsync,
                // Tells wasm to resize the window according to the available canvas
                fit_canvas_to_parent: true,
                // Tells wasm not to override default event handling, like F5, Ctrl+R etc.
                prevent_default_event_handling: false,
                // window_theme: Some(WindowTheme::Dark),
                ..default()
            }),
            ..default()
        }))
        .add_plugins(AskyPlugin)
        .add_systems(Update, bevy::window::close_on_esc)
        .add_systems(Startup, setup)
        .add_systems(Startup, ask_question)
        // .add_systems(Update, response)
        // .add_systems(Update, handle_tasks::<()>)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    let settings = BevyAskySettings {
        style: TextStyle {
            font: asset_server.load("fonts/DejaVuSansMono.ttf"),
            font_size: 50.0,
            color: Color::WHITE,
        },
    };
    commands.insert_resource(settings);
    commands.spawn(Camera2dBundle::default());
}

// fn ask_name<'a>(mut prompt: Prompt) -> impl Future<Output = ()> {
//     async move {
//         if let Ok(first_name) = prompt.read::<String>("What's your first name? ").await {
//             if let Ok(last_name) = prompt.read::<String>("What's your last name? ").await {
//                 prompt.message(format!("Hello, {first_name} {last_name}!"));
//             }
//         } else {
//             eprintln!("Got err in ask name");
//         }
//     }
// }

fn ask_question(mut asky: Asky, mut commands: Commands) {
    let confirm: Confirm<'static> = Confirm::new("Do you like coffee?");
    let waiter = asky.listen(confirm);
    let thread_pool = AsyncComputeTaskPool::get();
    let task = thread_pool.spawn(async move {
        let msg = match waiter.await {
            Ok(yes) => {
                if yes {
                    "Great, me too."
                } else {
                    "Oh, ok."
                }
            },
            Err(_) => "Uh oh, had a problem.",
        };
        println!("{}", msg);
    });
    commands.spawn(OnComplete(task));
}

fn handle_tasks<T: Send + 'static>(
    mut commands: Commands,
    mut transform_tasks: Query<(Entity, &mut OnComplete<T>)>,
) {
    for (entity, mut task) in &mut transform_tasks {
        if let Some(_) = block_on(future::poll_once(&mut task.0)) {

            // Task is complete, so remove task component from entity
            commands.entity(entity).remove::<OnComplete<T>>();
        }
    }
}