# html! Macro

The html! macro creates Bevy UI using HTML-like syntax. It allows for a declarative, structured way to define UI components converted to ECS code at compile time.

## Tag Names and Styling

Tag names are optional.

To style or query elements later in your Bevy code, assign a tag name.

To define a style, use the tag nam. You can optionally put the associated bevy component's visibility before writing the tag name.

## Example

```
html!(
    <Info>
        <>"Hello,"</>
        <>"World"</>
    </Info>
);
html!(
    <head>
    <style>
        .odd {
            Node {
                flex_direction: FlexDirection::Column,
                ..default()
            };
        }
        .even {
            Node {
                flex_direction: FlexDirection::Row,
                ..default()
            };
        }
    </style>
    </head>

    <Container>
        <Info class="odd" />
        <Info class="even" />
        <Info class="odd" />
    </Container>
);
App.add_systems(Startup, Container::spawn_as_root);
```

## What It Generates

It transforms the HTML into ECS code that defines components and spawns nodes with the specified style and attributes.

## What It Expands To

The html! macro takes the HTML-like syntax and generates native ECS code for Bevy. For the example above, it expands to this:

```
#[derive(Component)]
#[allow(dead_code)]
struct Info;

impl Info {
    #![allow(unused_variables)]
    fn spawn_as_child<'a>(parent: &'a mut ChildBuilder<'_>) -> EntityCommands<'a> {
        parent.spawn((Self, Self::get_node()))
    }
    fn get_node() -> Node {
        Node::default()
    }
    fn apply_attributes<'a>(
        mut me: EntityCommands<'a>,
        asset_server: &'a Res<AssetServer>,
    ) -> EntityCommands<'a> {
        me
    }
}
impl Info {
    fn spawn_as_root(mut commands: Commands, asset_server: Res<AssetServer>) {
        let entity = commands.spawn((Self, Self::get_node())).id();
        let mut me = commands.entity(entity);
        Self::spawn_children(&mut me, &asset_server);
        Self::apply_attributes(me, &asset_server);
    }
    fn spawn<'a>(
        parent: &'a mut ChildBuilder<'_>,
        asset_server: &'a Res<AssetServer>,
    ) -> EntityCommands<'a> {
        let mut me = Self::apply_attributes(parent.spawn((Self, Self::get_node())), asset_server);
        Self::spawn_children(&mut me, asset_server);
        me
    }
    fn spawn_children(me: &mut EntityCommands<'_>, asset_server: &Res<AssetServer>) {
        me.with_children(|parent| {
            parent.spawn(Node::default()).insert(Text::from("Hello,"));
            parent.spawn(Node::default()).insert(Text::from("World"));
        });
    }
}
#[derive(Component)]
#[allow(dead_code)]
struct Container;

impl Container {
    #![allow(unused_variables)]
    fn spawn_as_child<'a>(parent: &'a mut ChildBuilder<'_>) -> EntityCommands<'a> {
        parent.spawn((Self, Self::get_node()))
    }
    fn get_node() -> Node {
        Node::default()
    }
    fn apply_attributes<'a>(
        mut me: EntityCommands<'a>,
        asset_server: &'a Res<AssetServer>,
    ) -> EntityCommands<'a> {
        me
    }
}
#[allow(unused_macros)]
macro_rules! apply_orange_class {
    ($element:expr) => {{
        let mut element = $element;
        element.insert(TextColor::from(ORANGE_300));
        element
    }};
}
#[allow(unused_macros)]
macro_rules! apply_odd_class {
    ($element:expr) => {{
        let mut element = $element;
        element.insert(Node {
            flex_direction: FlexDirection::Column,
            ..default()
        });
        element
    }};
}
#[allow(unused_macros)]
macro_rules! apply_even_class {
    ($element:expr) => {{
        let mut element = $element;
        element.insert(Node {
            flex_direction: FlexDirection::Row,
            ..default()
        });
        element
    }};
}
impl Container {
    fn spawn_as_root(mut commands: Commands, asset_server: Res<AssetServer>) {
        let entity = commands.spawn((Self, Self::get_node())).id();
        let mut me = commands.entity(entity);
        Self::spawn_children(&mut me, &asset_server);
        Self::apply_attributes(me, &asset_server);
    }
    fn spawn<'a>(
        parent: &'a mut ChildBuilder<'_>,
        asset_server: &'a Res<AssetServer>,
    ) -> EntityCommands<'a> {
        let mut me = Self::apply_attributes(parent.spawn((Self, Self::get_node())), asset_server);
        Self::spawn_children(&mut me, asset_server);
        me
    }
    fn spawn_children(me: &mut EntityCommands<'_>, asset_server: &Res<AssetServer>) {
        me.with_children(|parent| {
            Info::apply_attributes(
                apply_odd_class!(Info::spawn(parent, asset_server)),
                &asset_server,
            );
            Info::apply_attributes(
                apply_even_class!(Info::spawn(parent, asset_server)),
                &asset_server,
            );
            Info::apply_attributes(
                apply_odd_class!(Info::spawn(parent, asset_server)),
                &asset_server,
            );
        });
    }
}
```
