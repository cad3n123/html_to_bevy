use convert_case::{Case, Casing};
use proc_macro::{token_stream, Delimiter, TokenStream, TokenTree};
use std::{
    collections::{HashMap, HashSet},
    iter::Peekable,
};

#[macro_use]
mod macros;

extern crate proc_macro;

type StructName = String;
struct StructInfo {
    visibility: Option<String>,
    node: Option<String>,
    attributes: Vec<String>,
}
type ClassName = String;
struct ClassInfo {
    visibility: Option<String>,
    attributes: Vec<String>,
}
type StyleInfo = (
    HashMap<StructName, StructInfo>,
    HashMap<ClassName, ClassInfo>,
);

/// Creates bevy ui from an html like syntax.
///
/// Tag names are optional, and allow you to style elements and query them in other parts of your bevy code. Classes allow you to apply the same style to multiple elements.
///
/// # Example
/// ```rust
/// App::new()
///     .add_plugins(DefaultPlugins)
///     .add_systems(Startup, (setup, Body::spawn).chain())
///     .run();
///
/// html!(
///     <head>
///     <style>
///         pub(crate) Container {
///             Node {
///                 flex_direction: FlexDirection::Column,
///                 ..default()
///             };
///         }
///         Line {
///             TextColor::from(ORANGE_300);
///         }
///         .odd {
///             TextFont::from_font_size(30.);
///         }
///         .even {
///             TextFont::from_font_size(40.);
///         }
///     </style>
///     </head>
///     
///     <body>
///     <Container>
///         <Line class="odd">"Line 1"</Line>
///         <Line class="even">"Line 2"</Line>
///         <Line class="odd">"Line 3"</Line>
///         <Line class="even">"Line 4"</Line>
///     </Container>
///     </body>
/// );
///
/// fn setup(mut commands: Commands) {
///     commands.spawn(Camera2d);
/// }
/// ```
///
/// # Panics
///
/// Panics if unable to parse macro result
#[proc_macro]
pub fn html(input: TokenStream) -> TokenStream {
    let mut tokens = input.into_iter().peekable();

    let structs_used = get_structs_used(tokens.clone());

    let styles = match parse_head(&mut tokens) {
        Ok(styles) => styles,
        Err(err) => return err,
    };

    let mut result = implement_styles(&structs_used, &styles);

    match parse_body(&mut tokens, &styles.1.into_keys().collect()) {
        Ok(body_result) => result.push_str(&body_result),
        Err(err) => return err,
    };

    result.parse().expect("Unable to parse macro result")
}
fn get_structs_used(
    mut tokens: Peekable<impl Iterator<Item = TokenTree> + Clone>,
) -> HashSet<String> {
    let mut struct_names = HashSet::new();
    while tokens.peek().is_some() {
        #[allow(clippy::unused_peekable)]
        let mut cloned = tokens.clone();
        if let (Some(TokenTree::Punct(first)), Some(TokenTree::Ident(second))) =
            (cloned.next(), cloned.next())
        {
            if first.as_char() == '<' {
                let struct_name = second.to_string();
                if !["head", "style", "body"].contains(&struct_name.as_str()) {
                    struct_names.insert(struct_name.to_case(Case::Pascal));
                }
            }
        }
        tokens.next();
    }
    struct_names
}
#[allow(clippy::too_many_lines)]
fn parse_head(tokens: &mut Peekable<token_stream::IntoIter>) -> Result<StyleInfo, TokenStream> {
    let mut structs: HashMap<StructName, StructInfo> = HashMap::new();
    let mut classes: HashMap<ClassName, ClassInfo> = HashMap::new();

    assert_next_tag(tokens, "head")?;
    if peek_matches_tag(tokens.clone(), "style") {
        tokens.nth(2);

        while let Some(token) = tokens.peek() {
            match token {
                TokenTree::Punct(p) => match p.as_char() {
                    '<' => {
                        break;
                    }
                    '.' => {
                        tokens.next();
                        let ident = assert_next_token!(tokens, Ident, Err).to_string();

                        let (class_name, visibility) = if ident == "pub" {
                            if peek_matches_token!(tokens, Group, Delimiter::Parenthesis) {
                                let visibility =
                                    unsafe { tokens.next().unwrap_unchecked().to_string() };
                                (
                                    assert_next_token!(tokens, Ident, Err).to_string(),
                                    Some(format!("{ident}{visibility}")),
                                )
                            } else {
                                (
                                    assert_next_token!(tokens, Ident, Err).to_string(),
                                    Some(ident),
                                )
                            }
                        } else {
                            (ident, None)
                        };

                        let TokenTree::Group(group) =
                            assert_next_token!(tokens, Group, Delimiter::Brace, Err)
                        else {
                            unreachable!()
                        };
                        let mut group_tokens = group.stream().into_iter().peekable();
                        let mut attributes = vec![];
                        while group_tokens.peek().is_some() {
                            let attribute = collect_until_token!(group_tokens, Punct, ";");
                            assert_next_token!(group_tokens, Punct, ";", Err);
                            attributes.push(attribute);
                        }
                        classes.insert(
                            class_name,
                            ClassInfo {
                                visibility,
                                attributes,
                            },
                        );
                    }
                    unexpected => {
                        return Err(format_compile_error!(
                            "Unexpected value {unexpected} in style"
                        ))
                    }
                },
                TokenTree::Ident(ident) => {
                    let ident = ident.to_string();
                    tokens.next();

                    let (struct_name, visibility) = if ident == "pub" {
                        if peek_matches_token!(tokens, Group, Delimiter::Parenthesis) {
                            let visibility =
                                unsafe { tokens.next().unwrap_unchecked().to_string() };
                            (
                                assert_next_token!(tokens, Ident, Err).to_string(),
                                Some(format!("{ident}{visibility}")),
                            )
                        } else {
                            (
                                assert_next_token!(tokens, Ident, Err).to_string(),
                                Some(ident),
                            )
                        }
                    } else {
                        (ident, None)
                    };

                    let TokenTree::Group(group) =
                        assert_next_token!(tokens, Group, Delimiter::Brace, Err)
                    else {
                        unreachable!()
                    };
                    let mut group_tokens = group.stream().into_iter().peekable();
                    let mut node = None;
                    let mut attributes = vec![];
                    while group_tokens.peek().is_some() {
                        let attribute = collect_until_token!(group_tokens, Punct, ";");
                        assert_next_token!(group_tokens, Punct, ";", Err);
                        if &attribute[.."Node".len()] == "Node" {
                            node = Some(attribute);
                        } else {
                            attributes.push(attribute);
                        }
                    }
                    structs.insert(
                        struct_name,
                        StructInfo {
                            visibility,
                            node,
                            attributes,
                        },
                    );
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
fn implement_styles(structs_used: &HashSet<String>, styles: &StyleInfo) -> String {
    let mut result = String::new();
    let (structs, classes) = styles;

    for struct_used in structs_used {
        let (visibility, node, attributes) = structs.get(struct_used).map_or_else(
            || (String::new(), "Node::default()".to_owned(), String::new()),
            |style| {
                let visibility = style
                    .visibility
                    .clone()
                    .map_or_else(<_>::default, |visibility| format!("{visibility} "));

                let node = style
                    .node
                    .as_ref()
                    .map_or_else(|| "Node::default()".to_owned(), ToOwned::to_owned);

                let mut attributes = String::new();

                if !style.attributes.is_empty() {
                    attributes.push_str("me");
                    for attribute in &style.attributes {
                        attributes.push_str(&format!(".insert({attribute})"));
                    }
                    attributes.push(';');
                }

                (visibility, node, attributes)
            },
        );

        result.push_str(&format!(
            "
            #[derive(Component)]\n
            {visibility}struct {struct_used};\n
            impl {struct_used} {{\n
                fn spawn<'a>(parent: &'a mut ChildBuilder<'_>) -> EntityCommands<'a> {{\n
                    parent.spawn((Self, {node}))
                }}\n
                fn apply_attributes<'a>(mut me: EntityCommands<'a>, asset_server: &'a Res<AssetServer>) -> EntityCommands<'a> {{\n
                    {attributes}
                    me\n
                }}
            }}"
        ));
    }
    for (class_name, class_info) in classes {
        let (visibility, attributes) = {
            let visibility = class_info
                .visibility
                .clone()
                .map_or_else(<_>::default, |visibility| format!("{visibility} "));

            let mut attributes = String::new();

            if !class_info.attributes.is_empty() {
                attributes.push_str("element");
                for attribute in &class_info.attributes {
                    attributes.push_str(&format!(".insert({attribute})"));
                }
                attributes.push(';');
            }

            (visibility, attributes)
        };
        let macro_name = format!("apply_{class_name}_class");

        result.push_str(&format!(
            "
            macro_rules! {macro_name} {{\n
                ($element:expr) => {{{{\n
                    let mut element = $element;\n
                    {attributes}
                    element\n
                }}}}\n
            }}\n
            {visibility} use {macro_name};"
        ));
    }

    result
}
fn parse_body(tokens: &mut Peekable<token_stream::IntoIter>, implemented_classes: &Vec<ClassName>) -> Result<String, TokenStream> {
    let mut result =
        "#[derive(Component)]\nstruct Body;\nimpl Body{\nfn spawn(mut commands: Commands, asset_server: Res<AssetServer>) {\ncommands.spawn((Self, Node { width: Val::Percent(100.), height: Val::Percent(100.), ..default()})).with_children(|parent| {\n".to_owned();
    assert_next_tag(tokens, "body")?;

    while tokens
        .clone()
        .nth(1)
        .is_some_and(|token| token.to_string() != "/")
    {
        let tag_result = parse_tag(tokens, implemented_classes)?;
        result.push_str(&tag_result);
    }
    assert_next_end_tag(tokens, "body")?;
    result.push_str("});\n}\n}");

    Ok(result)
}
fn parse_tag(
    tokens: &mut Peekable<impl Iterator<Item = TokenTree> + Clone>, implemented_classes: &Vec<ClassName>
) -> Result<String, TokenStream> {
    let mut result = String::new();

    assert_next_token!(tokens, Punct, "<", Err);
    let (struct_name, classes) = if peek_matches_token!(tokens, Ident) {
        let ident = unsafe { tokens.peek().unwrap_unchecked() }.to_string();
        let struct_name = if ident == "class" {
            None
        } else {
            tokens.next();
            Some(ident)
        };
        let classes = if peek_matches_token!(tokens, Ident, "class") {
            tokens.next();
            assert_next_token!(tokens, Punct, "=", Err);
            let classes_string = assert_next_string_lit!(tokens, Err);
            Some(
                classes_string
                    .split_whitespace()
                    .map(str::to_string)
                    .collect::<Vec<_>>(),
            )
        } else {
            None
        };

        (struct_name, classes)
    } else {
        (None, None)
    };
    assert_next_token!(tokens, Punct, ">", Err);
    if peek_matches_token!(tokens, Literal) || peek_matches_token!(tokens, Punct, "<") {
        let (apply_classes, end_parenthesis) = if let Some(classes) = classes {
            let (mut apply_classes, mut end_parenthesis) = (String::new(), String::new());
            for class in classes {
                if !implemented_classes.contains(&class) {
                    return Err(format_compile_error!("Class \\\"{class}\\\" does not exist"));
                }

                apply_classes.push_str(&format!("apply_{class}_class!("));
                end_parenthesis.push(')');
            }

            (apply_classes, end_parenthesis)
        } else {
            (String::new(), String::new())
        };

        result.push_str(&struct_name.as_ref().map_or_else(
            || format!("{apply_classes}parent.spawn(Node::default()){end_parenthesis}"),
            |struct_name| {
                
                format!(
                    "{struct_name}::apply_attributes({apply_classes}{struct_name}::spawn(parent){end_parenthesis}, &asset_server)"
                )
            },
        ));
        if peek_matches_token!(tokens, Literal) {
            let literal = unsafe { tokens.next().unwrap_unchecked() };
            result.push_str(&format!(".insert(Text::from({literal}))"));
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
            result.push_str("})");
        }
        result.push(';');
        assert_next_token!(tokens, Punct, "<", Err);
        assert_next_token!(tokens, Punct, "/", Err);
        if peek_matches_token!(tokens, Ident) {
            let tag = collect_until_token!(tokens, Punct, ">").to_case(Case::Pascal);
            if struct_name
                .clone()
                .is_none_or(|struct_name| tag != struct_name)
            {
                return Err(format_compile_error!(
                    "Expected </{}>",
                    struct_name.unwrap_or_default().to_case(Case::Kebab)
                ));
            }
        }
        assert_next_token!(tokens, Punct, ">", Err);
    } else {
        return Err(format_compile_error!("Expected end tag"));
    }

    Ok(result)
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
