use convert_case::{Case, Casing};
use proc_macro::{token_stream, Delimiter, TokenStream, TokenTree};
use std::iter::Peekable;

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
    Vec<(StructName, StructInfo)>,
    Vec<(ClassName, ClassInfo)>,
);

/// Creates bevy ui from an html like syntax.
///
/// Tag names are optional, and allow you to style elements and query those elements in other parts of your bevy code. Classes allow you to apply the same style to multiple elements. Also, styles made from a macro call can be used in other macro calls. 
///
/// # Example
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
/// # Example
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
/// 
/// 
#[proc_macro]
pub fn html(input: TokenStream) -> TokenStream {
    let mut tokens = input.into_iter().peekable();

    let mut styles = match parse_head(&mut tokens) {
        Ok(styles) => styles,
        Err(err) => return err,
    };

    let root_tag = match get_root_tag(&mut tokens) {
        Ok(root_tag) => root_tag,
        Err(err) => return err,
    };

    if let Some(root_tag) = &root_tag {
        if !styles.0.iter().any(|struct_style| &struct_style.0 == root_tag) {
            styles.0.push((root_tag.clone(), StructInfo { visibility: None, node: None, attributes: vec![]}));
        }
    }

    let mut result = implement_styles(&styles);

    if let Some(root_tag) = root_tag {
        let root_visibility = styles.0.iter().find(|struct_style| struct_style.0 == root_tag).map(|struct_style| struct_style.1.visibility.clone()).unwrap_or_default();

        match parse_root(&mut tokens, &root_tag, root_visibility.as_deref(), &styles.1.iter().map(|class| class.0.as_str()).collect()) {
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

    if !peek_matches_tag(tokens.clone(), "head"){
        return Ok((structs, classes))
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
                            unreachable!()
                        };
                        let mut group_tokens = group.stream().into_iter().peekable();
                        let mut attributes = vec![];
                        while group_tokens.peek().is_some() {
                            let attribute = collect_until_token!(group_tokens, Punct, ";");
                            assert_next_token!(group_tokens, Punct, ";", Err);
                            attributes.push(attribute);
                        }
                        classes.push((
                            class_name,
                            ClassInfo {
                                visibility: visibility.take(),
                                attributes,
                            }),
                        );
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
                            let visibility_inside =
                                unsafe { tokens.next().unwrap_unchecked().to_string() };
                            
                                Some(format!("{ident}{visibility_inside}"))
                            
                        } else {
                                Some(ident)
                        }
                    }
                },
                // Tag
                TokenTree::Ident(struct_name) => {
                    let struct_name = struct_name.to_string();
                    tokens.next();

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
                    structs.push((
                        struct_name,
                        StructInfo {
                            visibility: visibility.take(),
                            node,
                            attributes,
                        },)
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

                {visibility}fn apply_attributes<'a>(mut me: EntityCommands<'a>, asset_server: &'a Res<AssetServer>) -> EntityCommands<'a> {{\n
                    {attributes}
                    me\n
                }}
            }}"
        ));
    }
    for (class_name, class_info) in classes {
        let macro_name = format!("apply_{class_name}_class");

        let (visibility, attributes) = {
            let visibility = class_info
                .visibility
                .clone()
                .map_or_else(<_>::default, |visibility| format!("{visibility} use {macro_name};"));

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

        result.push_str(&format!(
            "
            #[allow(unused_macros)]
            macro_rules! {macro_name} {{\n
                ($element:expr) => {{{{\n
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
fn get_root_tag(tokens: &mut Peekable<token_stream::IntoIter>) -> Result<Option<String>, TokenStream> {
    if tokens.peek().is_some() {
        assert_next_token!(tokens, Punct, "<", Err);
        let Some(tag_name) = tokens.next().map(|token| token.to_string()) else {
            return Err(format_compile_error!("Expected root tag name"));
        };
        assert_next_token!(tokens, Punct, ">", Err);
        Ok(Some(tag_name))
    } else {
        Ok(None)
    }
    
    
}
// TODO: Let root node just have string
fn parse_root(tokens: &mut Peekable<token_stream::IntoIter>, root_tag: &str, root_visibility: Option<&str>, implemented_classes: &Vec<&str>) -> Result<String, TokenStream> {
    let root_visibility = root_visibility.unwrap_or_default();
    let mut result = format!("
    impl {root_tag}{{\n
        {root_visibility}fn spawn_as_root(mut commands: Commands, asset_server: Res<AssetServer>) {{\n
            let entity = commands.spawn((Self, Self::get_node())).id();\n
            let mut me = commands.entity(entity);\n
            Self::spawn_children(&mut me, &asset_server);\n
            Self::apply_attributes(me, &asset_server);\n
        }}\n
        {root_visibility}fn spawn<'a>(parent: &'a mut ChildBuilder<'_>, asset_server: &'a Res<AssetServer>) -> EntityCommands<'a> {{\n
            let mut me = Self::apply_attributes(\n
                parent.spawn((Self, Self::get_node())),\n
                asset_server\n
            );\n
            Self::spawn_children(&mut me, asset_server);\n
            me\n
        }}\n
        {root_visibility}fn spawn_children(me: &mut EntityCommands<'_>, asset_server: &Res<AssetServer>) {{\n
            me.with_children(|parent| {{\n");
    

    while tokens
        .clone()
        .nth(1)
        .is_some_and(|token| token.to_string() != "/")
    {
        let tag_result = parse_tag(tokens, implemented_classes)?;
        result.push_str(&tag_result);
    }
    assert_next_end_tag(tokens, root_tag)?;
    result.push_str("});\n}\n}");

    Ok(result)
}
#[allow(clippy::too_many_lines)]
fn parse_tag(
    tokens: &mut Peekable<impl Iterator<Item = TokenTree> + Clone>, implemented_classes: &Vec<&str>
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
    let is_self_closing = match tokens.next() {
        Some(TokenTree::Punct(punct)) if [">", "/"].contains(&punct.to_string().as_ref()) => {
            punct.to_string() == "/"
        },
        _ => {return Err(format_compile_error!("Expected > or /{}.", struct_name.map_or_else(String::new, |struct_name| format!(" after {struct_name}"))));}
    };

    let (apply_classes, end_parenthesis) = if let Some(classes) = classes {
        let (mut apply_classes, mut end_parenthesis) = (String::new(), String::new());
        for class in classes {
            if !implemented_classes.contains(&class.as_ref()) {
                return Err(format_compile_error!("Class \\\"{class}\\\" does not exist"));
            }

            apply_classes.push_str(&format!("apply_{class}_class!("));
            end_parenthesis.push(')');
        }

        (apply_classes, end_parenthesis)
    } else {
        (String::new(), String::new())
    };

    if is_self_closing {
        let Some(struct_name) = struct_name else { return Err(format_compile_error!("Self closing tag must have a name"))};
        result.push_str(&format!(
            "{struct_name}::apply_attributes({apply_classes}{struct_name}::spawn(parent, asset_server){end_parenthesis}, &asset_server)"
        ));
    } else {
        if !(peek_matches_token!(tokens, Literal) || peek_matches_token!(tokens, Punct, "<")) {
            return Err(format_compile_error!("Expected end tag"));
        }

        result.push_str(&struct_name.as_ref().map_or_else(
            || format!("{apply_classes}parent.spawn(Node::default()){end_parenthesis}"),
            |struct_name| {
                
                format!(
                    "{struct_name}::apply_attributes({apply_classes}{struct_name}::spawn_as_child(parent){end_parenthesis}, &asset_server)"
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
                    struct_name.unwrap_or_default()
                ));
            }
        }
        
    }
    result.push(';');
    assert_next_token!(tokens, Punct, ">", Err);

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
