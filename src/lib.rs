use convert_case::{Case, Casing};
use proc_macro::{token_stream, Delimiter, TokenStream, TokenTree};
use std::{collections::HashMap, iter::Peekable};

#[macro_use]
mod macros;

extern crate proc_macro;

struct StructInfo {
    node: Option<String>,
    attributes: Vec<String>,
}

#[proc_macro]
#[allow(
    clippy::missing_panics_doc,
    clippy::too_many_lines,
    clippy::cognitive_complexity
)]
pub fn html(input: TokenStream) -> TokenStream {
    let mut tokens = input.into_iter().peekable();
    let styles = match parse_head(&mut tokens) {
        Ok(styles) => styles,
        Err(err) => return err,
    };

    let result = match parse_body(&mut tokens, &styles) {
        Ok(result) => result,
        Err(err) => return err,
    };

    result.parse().unwrap()
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
                        let struct_name = assert_next_token!(tokens, Ident, Err)
                            .to_string()
                            .to_case(Case::Pascal);
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
                        result.insert(struct_name, StructInfo { node, attributes });
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
fn parse_body(
    tokens: &mut Peekable<token_stream::IntoIter>,
    styles: &HashMap<StructName, StructInfo>,
) -> Result<String, TokenStream> {
    let mut struct_names: Vec<String> = Vec::new();
    let mut result =
        "#[derive(Component)]\nstruct Body;\nimpl Body{\nfn spawn(mut commands: Commands) {\ncommands.spawn((Self, Node::default())).with_children(|parent| {\n".to_owned();
    assert_next_tag(tokens, "body")?;

    while tokens
        .clone()
        .nth(1)
        .is_some_and(|token| token.to_string() != "/")
    {
        let (mut tag_struct_names, tag_result) = parse_tag(tokens)?;
        struct_names.append(&mut tag_struct_names);
        result.push_str(&tag_result);
    }
    assert_next_end_tag(tokens, "body")?;
    result.push_str("});\n}\n}");

    for struct_name in struct_names {
        result.push_str(&format!(
            "#[derive(Component)]\n
            struct {struct_name};\n
            impl {struct_name} {{\n
                fn spawn<'a>(parent: &'a mut ChildBuilder<'_>) -> EntityCommands<'a> {{\n
                    let mut me = parent.spawn((Self,"
        ));
        result.push_str(&styles.get(&struct_name).map_or_else(
            || "Node::default()))".to_owned(),
            |style| {
                let mut result = style
                    .node
                    .clone()
                    .unwrap_or_else(|| "Node::default()".to_owned());
                result.push_str("))");

                if !style.attributes.is_empty() {
                    result.push_str(";\nme");
                    for attribute in &style.attributes {
                        result.push_str(&format!(".insert({attribute})"));
                    }
                }

                result
            },
        ));
        result.push_str(";\nme");
        result.push_str(
            "}\n
            }\n",
        );
    }

    Ok(result)
}
type StructName = String;
fn parse_tag(
    tokens: &mut Peekable<token_stream::IntoIter>,
) -> Result<(Vec<StructName>, String), TokenStream> {
    let mut struct_names = Vec::new();
    let mut result = String::new();

    assert_next_token!(tokens, Punct, "<", Err);
    let struct_name = if peek_matches_token!(tokens, Ident) {
        let struct_name = unsafe {
            tokens
                .next()
                .unwrap_unchecked()
                .to_string()
                .to_case(Case::Pascal)
        };
        struct_names.push(struct_name.clone());
        Some(struct_name)
    } else {
        None
    };
    assert_next_token!(tokens, Punct, ">", Err);
    if peek_matches_token!(tokens, Literal) || peek_matches_token!(tokens, Punct, "<") {
        result.push_str(&struct_name.as_ref().map_or_else(
            || "parent.spawn(Node::default())".to_owned(),
            |struct_name| format!("{struct_name}::spawn(parent)"),
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
                    Ok((mut child_struct_names, child_result)) => {
                        struct_names.append(&mut child_struct_names);
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
            let tag = unsafe { tokens.next().unwrap_unchecked() };
            if struct_name
                .clone()
                .is_none_or(|struct_name| tag.to_string().to_case(Case::Pascal) != struct_name)
            {
                return Err(format_compile_error!(
                    "Expected </{}>",
                    struct_name.unwrap_or_default()
                ));
            }
        }
        assert_next_token!(tokens, Punct, ">", Err);
    } else {
        return Err(format_compile_error!("Expected end tag"));
    }

    Ok((struct_names, result))
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
