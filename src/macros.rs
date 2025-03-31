macro_rules! assert_next_token {
    ($tokens:ident, $($args:tt)*) => {
        {assert_peek_token!($tokens, $($args)*);
        $tokens.next().unwrap()}
    }
}
macro_rules! assert_peek_token {
    ($tokens:ident, $tree:ident, $err:expr) => {
        if let Some(token) = $tokens.peek() {
            if !matches!(token, TokenTree::$tree(_)) {
                return $err(format_compile_error!(
                    "Unexpected token {token}. Expected {}",
                    stringify!($tree)
                ));
            }
        } else {
            return $err(format_compile_error!("Expected '{}'", stringify!($tree)));
        }
    };
    ($tokens:ident, Group, $expected:expr, $err:expr) => {
        if let Some(token) = $tokens.peek() {
            if let TokenTree::Group(group) = token {
                if group.delimiter() != $expected {
                    return $err(format_compile_error!(
                        "Unexpected token {group}. Expected {:?}",
                        $expected
                    ));
                }
            } else {
                return $err(format_compile_error!(
                    "Unexpected token {token}. Expected {:?}",
                    $expected
                ));
            }
        } else {
            return $err(format_compile_error!("Expected {:?}", $expected));
        }
    };
    ($tokens:ident, $tree:ident, $expected:expr, $err:expr) => {
        if let Some(token) = $tokens.peek() {
            if let TokenTree::$tree(tree) = token {
                if tree.to_string() != $expected {
                    return $err(format_compile_error!(
                        "Unexpected token {tree}. Expected {}",
                        $expected
                    ));
                }
            } else {
                return $err(format_compile_error!(
                    "Unexpected token {token}. Expected {}",
                    $expected
                ));
            }
        } else {
            return $err(format_compile_error!("Expected {}", $expected));
        }
    };
}
macro_rules! peek_matches_token {
    ($tokens:expr, $tree:ident, $expected:expr) => {{
        if let Some(TokenTree::$tree(tree)) = $tokens.peek() {
            tree.to_string() == $expected
        } else {
            false
        }
    }};
    ($tokens:expr, $tree:ident) => {{
        matches!($tokens.peek(), Some(TokenTree::$tree(_)))
    }};
}
macro_rules! format_compile_error {
    ($($arg:tt)*) => {
        {format!("compile_error!(\"{}\");", format_args!($($arg)*)).parse().unwrap()}
    };
}
