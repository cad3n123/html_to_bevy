use convert_case::{Case, Casing};
use proc_macro::{token_stream, Delimiter, TokenStream, TokenTree};
use std::{
    collections::{HashMap, HashSet},
    iter::Peekable,
};

#[macro_use]
mod macros;

extern crate proc_macro;

struct StructInfo {
    visibility: Option<String>,
    node: Option<String>,
    attributes: Vec<String>,
}

/// Creates bevy ui from an html like syntax.
///
/// Tag names are optional, and allow you to style elements and query them in other parts of your bevy code. low you to style elements and query them in other parts of your bevy code. To style elements, treat their tag name as an id, by using a hashtag before its name.
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
///     <script>
///         #container {
///             Node {
///                 flex_direction: FlexDirection::Column,
///                 ..default()
///             };
///         }
///     </script>
///     </head>
///
///     <body>
///         <container>
///             <>"Line 1"</>
///             <>"Line 2"</>
///         </main>
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

    match parse_body(&mut tokens) {
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
                let mut struct_name = second.to_string();
                if !["head", "script", "body"].contains(&struct_name.as_str()) {
                    while cloned.peek().is_some() && !peek_matches_token!(cloned, Punct, ">") {
                        struct_name
                            .push_str(unsafe { &cloned.next().unwrap_unchecked().to_string() });
                    }
                    struct_names.insert(struct_name.to_case(Case::Pascal));
                }
            }
        }
        tokens.next();
    }
    struct_names
}
fn parse_head(
    tokens: &mut Peekable<token_stream::IntoIter>,
) -> Result<HashMap<StructName, StructInfo>, TokenStream> {
    let mut result: HashMap<StructName, StructInfo> = HashMap::new();

    assert_next_tag(tokens, "head")?;
    if peek_matches_tag(tokens.clone(), "script") {
        tokens.nth(2);

        while let Some(token) = tokens.peek() {
            match token {
                TokenTree::Punct(p) => match p.as_char() {
                    '<' => {
                        break;
                    }
                    '#' => {
                        tokens.next();
                        let struct_name = {
                            let mut struct_name =
                                assert_next_token!(tokens, Ident, Err).to_string();
                            while tokens.peek().is_some() && !peek_matches_token!(tokens, Group) {
                                struct_name.push_str(unsafe {
                                    &tokens.next().unwrap_unchecked().to_string()
                                });
                            }
                            struct_name.to_case(Case::Pascal)
                        };
                        let visibility = if let Some(TokenTree::Group(group)) = tokens.peek() {
                            if group.delimiter() == Delimiter::Parenthesis {
                                let Some(TokenTree::Group(group)) = tokens.next() else {
                                    unreachable!()
                                };

                                Some(
                                    group
                                        .stream()
                                        .into_iter()
                                        .map(|token| token.to_string())
                                        .collect::<String>(),
                                )
                            } else {
                                None
                            }
                        } else {
                            None
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
                            let mut attribute = String::new();
                            while group_tokens.peek().is_some()
                                && !peek_matches_token!(group_tokens, Punct, ";")
                            {
                                attribute.push_str(unsafe {
                                    &group_tokens.next().unwrap_unchecked().to_string()
                                });
                            }
                            assert_next_token!(group_tokens, Punct, ";", Err);
                            if &attribute[.."Node".len()] == "Node" {
                                node = Some(attribute);
                            } else {
                                attributes.push(attribute);
                            }
                        }
                        result.insert(
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
                },
                unexpected => {
                    return Err(format_compile_error!(
                        "Unexpected value {unexpected} in style"
                    ))
                }
            }
        }

        assert_next_end_tag(tokens, "script")?;
    }
    assert_next_end_tag(tokens, "head")?;

    Ok(result)
}
fn implement_styles(
    structs_used: &HashSet<String>,
    styles: &HashMap<String, StructInfo>,
) -> String {
    let mut result = String::new();

    for struct_used in structs_used {
        let (visibility, node, attributes) = styles.get(struct_used).map_or_else(
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
                fn spawn<'a>(parent: &'a mut ChildBuilder<'_>, asset_server: &Res<AssetServer>) -> EntityCommands<'a> {{\n
                    let mut me = parent.spawn((Self, {node}));
                    {attributes}
                    me
                }}\n
            }}"
        ));
    }

    result
}
fn parse_body(tokens: &mut Peekable<token_stream::IntoIter>) -> Result<String, TokenStream> {
    let mut result =
        "#[derive(Component)]\nstruct Body;\nimpl Body{\nfn spawn(mut commands: Commands, asset_server: Res<AssetServer>) {\ncommands.spawn((Self, Node { width: Val::Percent(100.), height: Val::Percent(100.), ..default()})).with_children(|parent| {\n".to_owned();
    assert_next_tag(tokens, "body")?;

    while tokens
        .clone()
        .nth(1)
        .is_some_and(|token| token.to_string() != "/")
    {
        let tag_result = parse_tag(tokens)?;
        result.push_str(&tag_result);
    }
    assert_next_end_tag(tokens, "body")?;
    result.push_str("});\n}\n}");

    Ok(result)
}
type StructName = String;
fn parse_tag(tokens: &mut Peekable<token_stream::IntoIter>) -> Result<String, TokenStream> {
    let mut result = String::new();

    assert_next_token!(tokens, Punct, "<", Err);
    let struct_name = if peek_matches_token!(tokens, Ident) {
        let mut struct_name = unsafe { tokens.next().unwrap_unchecked().to_string() };
        while tokens.peek().is_some() && !peek_matches_token!(tokens, Punct, ">") {
            struct_name.push_str(unsafe { &tokens.next().unwrap_unchecked().to_string() });
        }
        struct_name = struct_name.to_case(Case::Pascal);
        Some(struct_name)
    } else {
        None
    };
    assert_next_token!(tokens, Punct, ">", Err);
    if peek_matches_token!(tokens, Literal) || peek_matches_token!(tokens, Punct, "<") {
        result.push_str(&struct_name.as_ref().map_or_else(
            || "parent.spawn(Node::default())".to_owned(),
            |struct_name| format!("{struct_name}::spawn(parent, &asset_server)"),
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
                match parse_tag(tokens) {
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
            let tag = {
                let mut tag = unsafe { tokens.next().unwrap_unchecked().to_string() };
                while tokens.peek().is_some() && !peek_matches_token!(tokens, Punct, ">") {
                    tag.push_str(unsafe { &tokens.next().unwrap_unchecked().to_string() });
                }
                tag.to_case(Case::Pascal)
            };
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
