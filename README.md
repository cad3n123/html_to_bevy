# html! Macro
The html! macro creates Bevy UI using HTML-like syntax. It allows for a declarative, structured way to define UI components converted to ECS code at compile time.

## Tag Names and Styling
Tag names are optional.

To style or query elements later in your Bevy code, assign a tag name.

To define a style, use the tag name with a hash prefix (like an ID in CSS).

## Example
```
use bevy::prelude::*;
use bevy_html_macro::html;

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
        .add_systems(Startup, (setup, Body::spawn).chain())
        .run();
}

html!(
    <head>
        <script>
            #container {
                Node {
                    flex_direction: FlexDirection::Column,
                    ..default()
                };
            }
        </script>
    </head>

    <body>
        <container>
            <>"Line 1"</>
            <>"Line 2"</>
        </container>
    </body>
);

fn setup(mut commands: Commands) {
    commands.spawn(Camera2dBundle::default());
}
```
### Panics
This macro will panic at compile time if it fails to parse the input syntax.

## What It Generates
It transforms the HTML into ECS code that defines components and spawns nodes with the specified style and attributes.

## What It Expands To
The html! macro takes the HTML-like syntax and generates native ECS code for Bevy. For the example above, it expands to this:
```
#[derive(Component)]
struct Body;

impl Body {
    fn spawn(mut commands: Commands) {
        commands
            .spawn((Self, Node::default()))
            .with_children(|parent| {
                Container::spawn(parent).with_children(|parent| {
                    parent.spawn(Node::default()).insert(Text::from("Line 1"));
                    parent.spawn(Node::default()).insert(Text::from("Line 2"));
                });
            });
    }
}
#[derive(Component)]
struct Container;

impl Container {
    fn spawn<'a>(parent: &'a mut ChildBuilder<'_>) -> EntityCommands<'a> {
        let mut me = parent.spawn((
            Self,
            Node {
                flex_direction: FlexDirection::Column,
                ..default()
            },
        ));
        me
    }
}
```
