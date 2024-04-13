use asky::bevy::*;

use asky::{Confirm, Message, MultiSelect, Number, Password, Select, Toggle};

use bevy::{render::camera::ClearColorConfig, prelude::*, window::PresentMode};

fn main() {
    let mut args: Vec<String> = std::env::args().collect();
    let options = [
        "confirm",
        "toggle",
        "text",
        "number",
        "float",
        "select",
        "password",
        "multi-select",
        "message",
    ];
    if args.len() != 2 {
        eprintln!("Usage: bevy <{}>", options.join("|"));
        std::process::exit(1);
    } else if !options.contains(&args[1].as_str()) && &args[1] != "multi_select" {
        eprintln!("Invalid argument: {}", args[1]);
        eprintln!("Usage: bevy <{}>", options.join("|"));
        std::process::exit(1);
    }
    let kind: String = args.remove(1);
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                title: "Bevy Asky Example".into(),
                resolution: (600., 400.).into(),
                present_mode: PresentMode::AutoVsync,
                // Tells wasm to resize the window according to the available canvas
                // fit_canvas_to_parent: true,
                // Tells wasm not to override default event handling, like F5, Ctrl+R etc.
                prevent_default_event_handling: false,
                // window_theme: Some(WindowTheme::Dark),
                ..default()
            }),
            ..default()
        }))
        .add_plugins(AskyPlugin)
        .add_systems(Update, bevy::window::close_on_esc)
        .add_systems(
            Startup,
            move |commands: Commands, asset: Res<AssetServer>| {
                setup(commands, asset, kind.as_str());
            },
        )
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>, kind: &str) {
    let settings = AskyStyle::default()
        .with_text_style(TextStyle {
            font: asset_server.load("fonts/DejaVuSansMono.ttf"),
            font_size: 50.0,
            color: Color::WHITE,
        });
    commands.insert_resource(settings);
    commands.spawn(Camera2dBundle {
        camera: Camera {
            // disable clearing completely (pixels stay as they are)
            // (preserves output from previous frame or camera/pass)
            clear_color: ClearColorConfig::Custom(Color::BLACK),
            ..default()
        },
        ..Default::default()
    });
    let confirm: Confirm<'static> = Confirm::new("Hi?");
    let message: Message<'static> = Message::new("My message");
    let toggle: Toggle<'static> = Toggle::new("Hi?", ["Bye", "What?"]);
    let text_input: asky::Text<'static> = asky::Text::new("Hi?");
    let number: Number<'static, u8> = Number::new("Number?");
    let float: Number<'static, f32> = Number::new("Float?");
    let select: Select<'static, &'static str> =
        Select::new("Favorite animal?", ["dog", "cow", "cat"]);
    let password: Password<'static> = Password::new("Password: ");
    let multi_select: MultiSelect<'static, &'static str> =
        MultiSelect::new("Favorite animal?", ["dog", "cow", "cat"]);

    let node = NodeBundle {
        style: Style {
            flex_direction: FlexDirection::Column,
            ..default()
        },
        ..default()
    };
    match kind {
        "multi_select" | "multi-select" => {
            commands
                .spawn(node)
                .insert((AskyNode::new(multi_select, ) , AskyState::Waiting));
        }
        "select" => {
            commands
                .spawn(node)
                .insert((AskyNode::new(select, ) , AskyState::Waiting));
        }
        "confirm" => {
            commands
                .spawn(node)
                .insert((AskyNode::new(confirm, ) , AskyState::Waiting));
        }
        "toggle" => {
            commands
                .spawn(node)
                .insert((AskyNode::new(toggle, ) , AskyState::Waiting));
        }
        "text" => {
            commands
                .spawn(node)
                .insert((AskyNode::new(text_input, ) , AskyState::Waiting));
        }
        "password" => {
            commands
                .spawn(node)
                .insert((AskyNode::new(password, ) , AskyState::Waiting));
        }
        "float" => {
            commands
                .spawn(node)
                .insert((AskyNode::new(float, ) , AskyState::Waiting));
        }
        "number" => {
            commands
                .spawn(node)
                .insert((AskyNode::new(number, ) , AskyState::Waiting));
        }
        "message" => {
            commands
                .spawn(node)
                .insert((AskyNode::new(message, ) , AskyState::Waiting));
        }
        _ => todo!("Unexpected example requested '{kind}'."), // .insert((AskyNode::new(password, ) , AskyState::Waiting))
    }
}
