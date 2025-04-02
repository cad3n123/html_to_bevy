# html! Macro

The html! macro creates Bevy UI using HTML-like syntax. It allows for a declarative, structured way to define UI components converted to ECS code at compile time.

## Tag Names and Styling

Tag names are optional.

To style or query elements later in your Bevy code, assign a tag name.

To define a style, use the tag name with a hash prefix (like an ID in CSS). You can optionally put the associated bevy component's visibility in parenthesis after writing the tag name.

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
    <style>
        pub(crate) Container {
            Node {
                flex_direction: FlexDirection::Column,
                ..default()
            };
        }
        Line {
            TextColor::from(ORANGE_300);
        }
        pub .odd {
            TextFont::from_font_size(30.);
        }
        .even {
            TextFont::from_font_size(40.);
        }
    </style>
    </head>

    <body>
    <Container>
        <Line class="odd">"Line 1"</Line>
        <Line class="even">"Line 2"</Line>
        <Line class="odd">"Line 3"</Line>
        <Line class="even">"Line 4"</Line>
    </Container>
    </body>
);

fn setup(mut commands: Commands) {
    commands.spawn(Camera2d);
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
struct Line;

impl Line {
    fn spawn<'a>(
        parent: &'a mut ChildBuilder<'_>,
        asset_server: &Res<AssetServer>,
    ) -> EntityCommands<'a> {
        parent.spawn((Self, Node::default()))
    }
    fn apply_attributes(mut me: EntityCommands) -> EntityCommands {
        me.insert(TextColor::from(ORANGE_300));
        me
    }
}
#[derive(Component)]
pub(crate) struct Container;

impl Container {
    fn spawn<'a>(
        parent: &'a mut ChildBuilder<'_>,
        asset_server: &Res<AssetServer>,
    ) -> EntityCommands<'a> {
        parent.spawn((
            Self,
            Node {
                flex_direction: FlexDirection::Column,
                ..default()
            },
        ))
    }
    fn apply_attributes(mut me: EntityCommands) -> EntityCommands {
        me
    }
}
macro_rules! apply_even_class {
    ($element:expr) => {{
        let mut element = $element;
        element.insert(TextFont::from_font_size(40.));
        element
    }};
}
pub use apply_even_class;
macro_rules! apply_odd_class {
    ($element:expr) => {{
        let mut element = $element;
        element.insert(TextFont::from_font_size(30.));
        element
    }};
}
#[derive(Component)]
struct Body;

impl Body {
    fn spawn(mut commands: Commands, asset_server: Res<AssetServer>) {
        commands
            .spawn((
                Self,
                Node {
                    width: Val::Percent(100.),
                    height: Val::Percent(100.),
                    ..default()
                },
            ))
            .with_children(|parent| {
                Container::apply_attributes(Container::spawn(parent, &asset_server)).with_children(
                    |parent| {
                        Line::apply_attributes(apply_odd_class!(Line::spawn(
                            parent,
                            &asset_server
                        )))
                        .insert(Text::from("Line 1"));
                        Line::apply_attributes(apply_even_class!(Line::spawn(
                            parent,
                            &asset_server
                        )))
                        .insert(Text::from("Line 2"));
                        Line::apply_attributes(apply_odd_class!(Line::spawn(
                            parent,
                            &asset_server
                        )))
                        .insert(Text::from("Line 3"));
                        Line::apply_attributes(apply_even_class!(Line::spawn(
                            parent,
                            &asset_server
                        )))
                        .insert(Text::from("Line 4"));
                    },
                );
            });
    }
}
```
