
use proc_macro::{token_stream, Delimiter, TokenStream, TokenTree};
use std::iter::Peekable;

#[macro_use]
mod macros;

extern crate proc_macro;

type StructName = String;
#[derive(Default)]
struct StructInfo {
    visibility: Option<String>,
    node: Option<String>,
    attributes: Vec<String>,
    vars: Vec<VarInfo>,
}
type ClassName = String;
struct ClassInfo {
    visibility: Option<String>,
    attributes: Vec<String>,
    vars: Vec<VarInfo>,
}
type StyleInfo = (Vec<(StructName, StructInfo)>, Vec<(ClassName, ClassInfo)>);
type AttributeName = String;
type AttributeValue = String;

/// Creates bevy ui from an html like syntax.
///
/// Tag names are optional, and allow you to style elements and query those elements in other parts of your bevy code. Classes allow you to apply the same style to multiple elements. Also, styles made from a macro call can be used in other macro calls.
///
/// ## Example
/// ```rust
/// html!(
///     <Container>
///         <>"Hello, World!"</>
///     </Container>
/// );
/// App.add_systems(Startup, Container::spawn_as_root);
/// ```
/// # Reusing Roots
/// Root elements can be used as elements in other macro calls, by formatting it as a self closing tag.
///
/// ## Example
/// ```rust
/// html!(
///     <Info>
///         <>"Hello,"</>
///         <>"World"</>
///     </Info>
/// );
/// html!(
///     <head>
///     <style>
///         .odd {
///             Node {
///                 flex_direction: FlexDirection::Column,
///                 ..default()
///             };
///         }
///         .even {
///             Node {
///                 flex_direction: FlexDirection::Row,
///                 ..default()
///             };
///         }
///     </style>
///     </head>
///
///     <Container>
///         <Info class="odd" />
///         <Info class="even" />
///         <Info class="odd" />
///     </Container>
/// );
/// App.add_systems(Startup, Container::spawn_as_root);
/// ```
/// # Style Variables
/// String variables can be used when styling elements to make them more reusable. Elements inherit their parent's variable values, unless the value is reset.
/// ## Example
/// In this example, a generic `User` tag is made, with children `FName` and `LName`. `FName` and `LName` use variables in their text content, but in other parts of the code, these value can be set on the `User` tag.
/// ```rust
/// html!(
/// <head>
/// <style>
///     User {
///     }
///     Name {
///         Node {
///             column_gap: Val::Px(10.),
///             ..default()
///         };
///     }
///     FName {
///         $fname = "Error";
///
///         Text::from(String::from($fname));
///     }
///     LName {
///         $lname = "Error";
///
///         Text::from(String::from($lname));
///     }
/// </style>
/// </head>
/// <User>
///     <Name>
///         <FName></FName><LName></LName>
///     </Name>
/// </User>
/// );
/// html!(
/// <head>
/// <style>
/// Container {
///     Node {
///         flex_direction: FlexDirection::Column,
///         ..default()
///     };
/// }
/// </style>
/// </head>
///
/// <Container>
///     <User fname="John" lname="Smith" />
///     <User fname="Jane" lname="Smith" />
/// </Container>
/// );
/// ```
///
#[proc_macro]
pub fn html(input: TokenStream) -> TokenStream {
    let mut tokens = input.into_iter().peekable();

    let mut styles: (Vec<(String, StructInfo)>, Vec<(String, ClassInfo)>) =
        match parse_head(&mut tokens) {
            Ok(styles) => styles,
            Err(err) => return err,
        };

    let root = match get_root_tag(&mut tokens) {
        Ok(root_tag) => root_tag,
        Err(err) => return err,
    };

    if let Some((root_tag, _root_attributes)) = &root {
        if !styles
            .0
            .iter()
            .any(|struct_style| &struct_style.0 == root_tag)
        {
            styles.0.push((root_tag.clone(), StructInfo::default()));
        }
    }

    let mut result = implement_styles(&styles);

    if let Some((root_tag, mut root_attributes)) = root {
        let root_visibility = styles
            .0
            .iter()
            .find(|struct_style| struct_style.0 == root_tag)
            .map(|struct_style| struct_style.1.visibility.clone())
            .unwrap_or_default();

        match parse_root(
            &mut tokens,
            &root_tag,
            &mut root_attributes,
            root_visibility.as_deref(),
            &styles.1.iter().map(|class| class.0.as_str()).collect(),
        ) {
            Ok(body_result) => result.push_str(&body_result),
            Err(err) => return err,
        };
    }

    match result.parse() {
        Ok(result) => result,
        Err(err) => format_compile_error!("Could not parse result: {err}"),
    }
}
#[allow(clippy::too_many_lines)]
fn parse_head(tokens: &mut Peekable<token_stream::IntoIter>) -> Result<StyleInfo, TokenStream> {
    let mut structs: Vec<(StructName, StructInfo)> = Vec::new();
    let mut classes: Vec<(ClassName, ClassInfo)> = Vec::new();
    let mut visibility = None;

    if !peek_matches_tag(tokens.clone(), "head") {
        return Ok((structs, classes));
    }
    assert_next_tag(tokens, "head")?;
    if peek_matches_tag(tokens.clone(), "style") {
        tokens.nth(2);

        while let Some(token) = tokens.peek() {
            match token {
                TokenTree::Punct(p) => match p.as_char() {
                    '<' => {
                        break;
                    }
                    // Class
                    '.' => {
                        tokens.next();
                        let class_name = assert_next_token!(tokens, Ident, Err).to_string();

                        let TokenTree::Group(group) =
                            assert_next_token!(tokens, Group, Delimiter::Brace, Err)
                        else {
                            return Err(format_compile_error!("Not reachable"));
                        };
                        let mut group_tokens = group.stream().into_iter().peekable();
                        let mut attributes = vec![];
                        let mut vars = vec![];
                        while group_tokens.peek().is_some() {
                            if peek_matches_token!(group_tokens, Punct, "$") {
                                vars.push(parse_local_var(&mut group_tokens)?);
                            } else {
                                let attribute = parse_style_attribute(&mut group_tokens, "$")?;
                                assert_next_token!(group_tokens, Punct, ";", Err);
                                attributes.push(attribute);
                            }
                        }
                        classes.push((
                            class_name,
                            ClassInfo {
                                visibility: visibility.take(),
                                attributes,
                                vars,
                            },
                        ));
                    }
                    unexpected => {
                        return Err(format_compile_error!(
                            "Unexpected value {unexpected} in style"
                        ))
                    }
                },
                // Visibility
                TokenTree::Ident(ident) if ident.to_string() == "pub" => {
                    let ident = ident.to_string();
                    tokens.next();
                    visibility = {
                        if peek_matches_token!(tokens, Group, Delimiter::Parenthesis) {
                            let Some(visibility_inside) = tokens.next() else {
                                return Err(format_compile_error!("Unreachable 4"));
                            };

                            Some(format!("{ident}{visibility_inside}"))
                        } else {
                            Some(ident)
                        }
                    }
                }
                // Tag
                TokenTree::Ident(struct_name) => {
                    let struct_name = struct_name.to_string();
                    tokens.next();

                    let TokenTree::Group(group) =
                        assert_next_token!(tokens, Group, Delimiter::Brace, Err)
                    else {
                        return Err(format_compile_error!("Not reachable"));
                    };
                    let mut group_tokens = group.stream().into_iter().peekable();
                    let mut node = None;
                    let mut attributes = vec![];
                    let mut vars = vec![];
                    while group_tokens.peek().is_some() {
                        if peek_matches_token!(group_tokens, Punct, "$") {
                            vars.push(parse_local_var(&mut group_tokens)?);
                        } else {
                            let attribute = parse_style_attribute(&mut group_tokens, "")?;
                            assert_next_token!(group_tokens, Punct, ";", Err);
                            if &attribute[.."Node".len()] == "Node" {
                                node = Some(attribute);
                            } else {
                                attributes.push(attribute);
                            }
                        }
                    }
                    structs.push((
                        struct_name,
                        StructInfo {
                            visibility: visibility.take(),
                            node,
                            attributes,
                            vars,
                        },
                    ));
                }
                unexpected => {
                    return Err(format_compile_error!(
                        "Unexpected value {unexpected} in style"
                    ))
                }
            }
        }

        assert_next_end_tag(tokens, "style")?;
    }
    assert_next_end_tag(tokens, "head")?;

    Ok((structs, classes))
}
struct VarInfo {
    name: String,
    value: String,
}
fn parse_local_var(tokens: &mut Peekable<token_stream::IntoIter>) -> Result<VarInfo, TokenStream> {
    assert_next_token!(tokens, Punct, "$", Err);
    let name = assert_next_token!(tokens, Ident, Err).to_string();
    assert_next_token!(tokens, Punct, "=", Err);
    let value = assert_next_string_lit!(tokens, Err);
    assert_next_token!(tokens, Punct, ";", Err);

    Ok(VarInfo { name, value })
}
const fn delimiter_to_chars(delimiter: Delimiter) -> (char, char) {
    match delimiter {
        Delimiter::Brace => ('{', '}'),
        Delimiter::Parenthesis => ('(', ')'),
        Delimiter::Bracket => ('[', ']'),
        Delimiter::None => (' ', ' '),
    }
}
fn parse_style_attribute(
    group_tokens: &mut Peekable<token_stream::IntoIter>,
    attributes_prefix: &str,
) -> Result<String, TokenStream> {
    let mut token_stack = vec![];
    let mut group_delimiter_stack = vec![];
    let attribute = {
        let mut collected = String::new();
        while token_stack.last().is_some()
            || (group_tokens.peek().is_some() && !peek_matches_token!(group_tokens, Punct, ";"))
        {
            let tokens = token_stack.last_mut().unwrap_or(group_tokens);
            if peek_matches_token!(tokens, Group) {
                let Some(TokenTree::Group(group)) = tokens.next() else {
                    return Err(format_compile_error!("Unreachable 5"));
                };
                let delimiter = group.delimiter();
                group_delimiter_stack.push(delimiter);
                collected.push(delimiter_to_chars(delimiter).0);

                token_stack.push(group.stream().into_iter().peekable());
            } else if peek_matches_token!(tokens, Punct, "$") {
                tokens.next();
                let var = assert_next_token!(tokens, Ident, Err);
                collected.push_str(&format!(
                    "{attributes_prefix}attributes.get(\"{var}\").unwrap_or(&{var})"
                ));
            } else {
                collected.push_str(&if let Some(token) = tokens.next() {
                    token.to_string()
                } else {
                    return Err(format_compile_error!("Unreachable 6"));
                });
            }
            while token_stack
                .last_mut()
                .is_some_and(|tokens| tokens.peek().is_none())
            {
                if let Some(delimiter) = group_delimiter_stack.pop() {
                    collected.push(delimiter_to_chars(delimiter).1);
                }

                token_stack.pop();
            }
        }
        collected
    };
    Ok(attribute)
}
fn implement_styles(styles: &StyleInfo) -> String {
    let mut result = String::new();
    let (structs, classes) = styles;

    for (struct_name, struct_info) in structs {
        let visibility = struct_info
            .visibility
            .clone()
            .map_or_else(<_>::default, |visibility| format!("{visibility} "));

        let node = struct_info
            .node
            .as_ref()
            .map_or_else(|| "Node::default()".to_owned(), ToOwned::to_owned);

        let mut attributes = String::new();

        if !struct_info.attributes.is_empty() {
            attributes.push_str("me");
            for attribute in &struct_info.attributes {
                attributes.push_str(&format!(".insert({attribute})"));
            }
            attributes.push(';');
        }

        let vars = {
            let mut vars = String::new();
            for var in &struct_info.vars {
                vars.push_str(&format!(
                    "let {}: String = String::from(\"{}\");\n",
                    var.name, var.value
                ));
            }
            vars
        };

        // Attributes (Variables) need to be passed into apply_attributes method
        result.push_str(&format!(
            "
            #[derive(Component)]\n
            #[allow(dead_code)]\n
            {visibility}struct {struct_name};\n
            impl {struct_name} {{\n
                #![allow(unused_variables)]\n

                {visibility}fn spawn_as_child<'a>(parent: &'a mut ChildBuilder<'_>) -> EntityCommands<'a> {{\n
                    parent.spawn((Self, Self::get_node()))
                }}\n

                {visibility}fn get_node() -> Node {{\n
                    {node}
                }}\n

                {visibility}fn apply_attributes<'a>(mut me: EntityCommands<'a>, asset_server: &'a Res<AssetServer>, attributes: &std::collections::HashMap<String,String>) -> EntityCommands<'a> {{\n
                    {vars}
                    {attributes}
                    me\n
                }}
            }}"
        ));
    }
    for (class_name, class_info) in classes {
        let macro_name = format!("apply_{class_name}_class");

        let (visibility, attributes, vars) = {
            let visibility = class_info
                .visibility
                .clone()
                .map_or_else(<_>::default, |visibility| {
                    format!("{visibility} use {macro_name};")
                });

            let mut attributes = String::new();

            if !class_info.attributes.is_empty() {
                attributes.push_str("element");
                for attribute in &class_info.attributes {
                    attributes.push_str(&format!(".insert({attribute})"));
                }
                attributes.push(';');
            }

            let vars = {
                let mut vars = String::new();
                for var in &class_info.vars {
                    vars.push_str(&format!(
                        "let {}: String = String::from(\"{}\");\n",
                        var.name, var.value
                    ));
                }
                vars
            };

            (visibility, attributes, vars)
        };

        result.push_str(&format!(
            "
            #[allow(unused_macros)]
            macro_rules! {macro_name} {{\n
                ($element:expr, $asset_server:ident, $attributes:ident) => {{{{\n
                    let asset_server = &$asset_server;
                    {vars}
                    #[allow(unused_mut)]
                    let mut element = $element;\n
                    {attributes}
                    element\n
                }}}}\n
            }}\n
            {visibility}"
        ));
    }

    result
}
fn get_root_tag(
    tokens: &mut Peekable<token_stream::IntoIter>,
) -> Result<Option<(String, Vec<(AttributeName, AttributeValue)>)>, TokenStream> {
    if tokens.peek().is_some() {
        assert_next_token!(tokens, Punct, "<", Err);
        let Some(tag_name) = tokens.next().map(|token| token.to_string()) else {
            return Err(format_compile_error!("Expected root tag name"));
        };
        let attribute_info = parse_attributes(tokens)?;
        assert_next_token!(tokens, Punct, ">", Err);
        Ok(Some((tag_name, attribute_info)))
    } else {
        Ok(None)
    }
}
fn parse_root(
    tokens: &mut Peekable<token_stream::IntoIter>,
    root_tag: &str,
    root_attributes: &mut Vec<(AttributeName, AttributeValue)>,
    root_visibility: Option<&str>,
    implemented_classes: &Vec<&str>,
) -> Result<String, TokenStream> {
    let root_visibility = root_visibility.unwrap_or_default();
    let classes = classes_from_attributes(root_attributes);
    let (apply_classes, apply_classes_end) = get_apply_classes_code(implemented_classes, classes)?;
    let attribute_tuples = get_attribute_tuples(root_attributes);

    let is_literal = peek_matches_token!(tokens, Literal);
    let literal_value = if is_literal {
        Some(assert_next_string_lit!(tokens, Err))
    } else {
        None
    };
    let literal_value_code = literal_value
        .map(|literal_value| format!(", Text::from(\"{literal_value}\")"))
        .unwrap_or_default();

    let mut result = format!("
    impl {root_tag}{{\n
        {root_visibility}fn spawn_as_root(mut commands: Commands, asset_server: Res<AssetServer>) {{\n
            let attributes: std::collections::HashMap<String, String> = std::collections::HashMap::from([{attribute_tuples}]);\n
            let entity = commands.spawn((Self, Self::get_node(){literal_value_code})).id();\n
            let mut me = commands.entity(entity);\n
            Self::spawn_children(&mut me, &asset_server, attributes.clone());\n
            Self::apply_attributes({apply_classes}me{apply_classes_end}, &asset_server, &attributes);\n
        }}\n
        {root_visibility}fn spawn<'a>(parent: &'a mut ChildBuilder<'_>, asset_server: &'a Res<AssetServer>, new_attributes: &std::collections::HashMap<String,String>) -> EntityCommands<'a> {{\n
            let mut attributes: std::collections::HashMap<String, String> = std::collections::HashMap::from([{attribute_tuples}]);\n
            for (k, v) in new_attributes {{\n
                attributes.insert(k.clone(), v.clone());\n
            }}\n
            let mut me = Self::apply_attributes(\n
                {apply_classes}parent.spawn((Self, Self::get_node(){literal_value_code})){apply_classes_end},\n
                asset_server,\n
                &attributes,\n
            );\n
            Self::spawn_children(&mut me, asset_server, attributes);\n
            me\n
        }}\n
        {root_visibility}fn spawn_children(me: &mut EntityCommands<'_>, asset_server: &Res<AssetServer>, attributes: std::collections::HashMap<String,String>) {{\n
            me.with_children(|parent| {{\n");
    if !is_literal {
        while tokens
            .clone()
            .nth(1)
            .is_some_and(|token| token.to_string() != "/")
        {
            let tag_result = parse_tag(tokens, implemented_classes)?;
            result.push_str(&tag_result);
        }
    }
    assert_next_end_tag(tokens, root_tag)?;
    result.push_str("});\n}\n}");

    Ok(result)
}

fn get_attribute_tuples(attributes: &[(String, String)]) -> String {
    let attribute_tuples = attributes
        .iter()
        .fold(String::new(), |mut result, attribute| {
            result.push_str(&format!(
                "(String::from(\"{}\"), String::from(\"{}\")),",
                attribute.0, attribute.1
            ));
            result
        });
    attribute_tuples
}
#[allow(clippy::too_many_lines)]
fn parse_tag(
    tokens: &mut Peekable<impl Iterator<Item = TokenTree> + Clone>,
    implemented_classes: &Vec<&str>,
) -> Result<String, TokenStream> {
    let mut result = String::new();

    assert_next_token!(tokens, Punct, "<", Err);
    let (struct_name, classes, attributes) = if peek_matches_token!(tokens, Ident) {
        let ident = if let Some(token) = tokens.peek() {
            token.to_string()
        } else {
            return Err(format_compile_error!("Unreachable 7"));
        };
        let struct_name = if tokens
            .clone()
            .nth(1)
            .is_some_and(|token| token.to_string() == "=")
        {
            None
        } else {
            tokens.next();
            Some(ident)
        };
        let mut attributes = parse_attributes(tokens)?;
        let classes = classes_from_attributes(&mut attributes);

        (struct_name, classes, attributes)
    } else {
        (None, None, vec![])
    };
    let is_self_closing = match tokens.next() {
        Some(TokenTree::Punct(punct)) if [">", "/"].contains(&punct.to_string().as_ref()) => {
            punct.to_string() == "/"
        }
        _ => {
            return Err(format_compile_error!(
                "Expected > or /{}.",
                struct_name.map_or_else(String::new, |struct_name| format!(" after {struct_name}"))
            ));
        }
    };

    let (apply_classes, apply_classes_end) = get_apply_classes_code(implemented_classes, classes)?;

    let attribute_tuples = get_attribute_tuples(&attributes);
    if is_self_closing {
        // Has no children
        let Some(struct_name) = struct_name else {
            return Err(format_compile_error!("Self closing tag must have a name"));
        };
        result.push_str(&format!("{{\n
            let attributes: std::collections::HashMap<String, String> = std::collections::HashMap::<String, String>::from([{attribute_tuples}]);\n
            {struct_name}::apply_attributes({apply_classes}{struct_name}::spawn(parent, asset_server, &attributes){apply_classes_end}, asset_server, &attributes)\n
        }};"));
    } else {
        // Potentially has children
        if !(peek_matches_token!(tokens, Literal) || peek_matches_token!(tokens, Punct, "<")) {
            return Err(format_compile_error!("Expected end tag"));
        }

        result.push_str(&struct_name.as_ref().map_or_else(
            || format!("{{{apply_classes}parent.spawn(Node::default()){apply_classes_end}"),
            |struct_name| {
                format!("{{\n
                    let mut attributes = attributes.clone();\n
                    for (k, v) in &std::collections::HashMap::<String, String>::from([{attribute_tuples}]) {{\n
                        attributes.insert(k.clone(), v.clone());\n
                    }}\n
                    {struct_name}::apply_attributes({apply_classes}{struct_name}::spawn_as_child(parent){apply_classes_end}, asset_server, &attributes)")
            },
        ));
        if peek_matches_token!(tokens, Literal) {
            let Some(literal) = tokens.next() else {
                return Err(format_compile_error!("Unreachable 8"));
            };
            result.push_str(&format!(".insert(Text::from({literal}));"));
        } else {
            result.push_str(".with_children(|parent| {\n");
            while tokens
                .clone()
                .nth(1)
                .is_some_and(|token| token.to_string() != "/")
            {
                match parse_tag(tokens, implemented_classes) {
                    Ok(child_result) => {
                        result.push_str(&child_result);
                    }
                    error => {
                        return error;
                    }
                }
            }
            result.push_str("});");
        }
        result.push('}');
        assert_next_token!(tokens, Punct, "<", Err);
        assert_next_token!(tokens, Punct, "/", Err);
        if peek_matches_token!(tokens, Ident) {
            let tag = collect_until_token!(tokens, Punct, ">");
            if struct_name
                .clone()
                .is_none_or(|struct_name| tag != struct_name)
            {
                return Err(format_compile_error!(
                    "Expected </{}>",
                    struct_name.unwrap_or_default()
                ));
            }
        }
    }
    assert_next_token!(tokens, Punct, ">", Err);

    Ok(result)
}

fn classes_from_attributes(attributes: &mut Vec<(String, String)>) -> Option<Vec<String>> {
    let class_info = attributes
        .iter()
        .position(|attribute| attribute.0 == "class");
    let classes = class_info.map(|class_info| {
        attributes
            .swap_remove(class_info)
            .1
            .split_whitespace()
            .map(str::to_string)
            .collect::<Vec<_>>()
    });
    classes
}
fn get_apply_classes_code(
    implemented_classes: &Vec<&str>,
    classes: Option<Vec<String>>,
) -> Result<(String, String), TokenStream> {
    let (apply_classes, apply_classes_end) = if let Some(classes) = classes {
        let (mut apply_classes, mut apply_classes_end) = (String::new(), String::new());
        for class in classes {
            if !implemented_classes.contains(&class.as_ref()) {
                return Err(format_compile_error!(
                    "Class \\\"{class}\\\" does not exist"
                ));
            }

            apply_classes.push_str(&format!("apply_{class}_class!("));
            apply_classes_end.push_str(", asset_server, attributes)");
        }

        (apply_classes, apply_classes_end)
    } else {
        (String::new(), String::new())
    };
    Ok((apply_classes, apply_classes_end))
}
fn peek_matches_tag(mut tokens: impl Iterator<Item = TokenTree>, expected: &str) -> bool {
    if let (Some(TokenTree::Punct(p1)), Some(TokenTree::Ident(tag)), Some(TokenTree::Punct(p2))) =
        (tokens.next(), tokens.next(), tokens.next())
    {
        p1.as_char() == '<' && p2.as_char() == '>' && tag.to_string() == expected
    } else {
        false
    }
}
fn assert_next_end_tag(
    tokens: &mut Peekable<impl Iterator<Item = TokenTree> + Clone>,
    expected: &str,
) -> Result<(), TokenStream> {
    let first = tokens.peek().map(ToString::to_string);
    let second = tokens.clone().nth(1).map(|token| token.to_string());
    match (tokens.next(), tokens.next(), tokens.next(), tokens.next()) {
        (
            Some(TokenTree::Punct(p1)),
            Some(TokenTree::Punct(p2)),
            Some(TokenTree::Ident(tag)),
            Some(TokenTree::Punct(p3)),
        ) if p1.as_char() == '<' && p2.as_char() == '/' && p3.as_char() == '>' => {
            if tag.to_string() != expected {
                return Err(format_compile_error!(
                    "Expected </{expected}>, but received </{tag}>",
                ));
            }
            Ok(())
        }
        _ => {
            if let Some(first) = first {
                return Err(format_compile_error!(
                    "Expected </{expected}>, but received {first}{}",
                    second.unwrap_or_default()
                ));
            }
            Err(format_compile_error!("Expected </{expected}>",))
        }
    }
}
fn assert_next_tag(
    tokens: &mut Peekable<impl Iterator<Item = TokenTree> + Clone>,
    expected: &str,
) -> Result<(), TokenStream> {
    match (tokens.next(), tokens.next(), tokens.next()) {
        (Some(TokenTree::Punct(p1)), Some(TokenTree::Ident(tag)), Some(TokenTree::Punct(p2)))
            if p1.as_char() == '<' && p2.as_char() == '>' =>
        {
            if tag.to_string() != expected {
                return Err(format_compile_error!(
                    "Expected <{expected}>, but received <{tag}>",
                ));
            }
            Ok(())
        }
        _ => Err(format_compile_error!("Expected <{expected}>",)),
    }
}
fn parse_attributes(
    tokens: &mut Peekable<impl Iterator<Item = TokenTree> + Clone>,
) -> Result<Vec<(AttributeName, AttributeValue)>, TokenStream> {
    let mut result = vec![];
    while !(peek_matches_token!(tokens, Punct, ">") || peek_matches_token!(tokens, Punct, "/")) {
        let attribute_name = assert_next_token!(tokens, Ident, Err).to_string();
        assert_next_token!(tokens, Punct, "=", Err);
        let attribute_value = assert_next_string_lit!(tokens, Err);
        result.push((attribute_name, attribute_value));
    }
    Ok(result)
}
