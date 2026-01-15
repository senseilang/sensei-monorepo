use crate::tests::assert_parser_errors;

#[test]
fn test_missing_semicolon() {
    assert_parser_errors(
        r#"const x =
            init {
                if (false) {
                    awesome = a == 5;
                }
            }
            "#,
        &["
                error: missing `;`
                  --> line 1:10
                   |
                  1| const x =
                   |          ^
            "],
    );
}

#[test]
fn test_unclosed_if() {
    assert_parser_errors(
        r#"run {
            if (wow) {
                my_awesome_statement(3 + a, nice);



        }"#,
        &["
                error: unexpected EOF, expected `}`
                  --> line 7:9
                   |
                  7|         }
                   |         ^
            "],
    );
}

#[test]
fn test_missing_open_run_block() {
    assert_parser_errors(
        r#"
            run }
            "#,
        &["
                error: unexpected `}`, expected `{`
                  --> line 2:17
                   |
                  2|             run }
                   |                 ^
            "],
    );
}
#[test]
fn test_missing_close_run_block() {
    assert_parser_errors(
        r#"
            run {
            "#,
        &["
                error: missing `}`
                  --> line 2:18
                   |
                  2|             run {
                   |                  ^
            "],
    );
}

#[test]
fn test_unexpected_token_at_top_level() {
    assert_parser_errors(
        "5;",
        &[
            "
                    error: unexpected decimal literal, expected one of `init`, `run`, `const`
                      --> line 1:1
                       |
                      1| 5;
                       | ^
                ",
            "
                    error: unexpected `;`, expected one of `init`, `run`, `const`
                      --> line 1:2
                       |
                      1| 5;
                       |  ^
                ",
        ],
    );
}

#[test]
fn test_unexpcted_token_post_const_decl() {
    assert_parser_errors(
        r#"const name run {}

            "#,
        &[r#"
                error: unexpected `run`, expected one of `:`, `=`
                  --> line 1:12
                   |
                  1| const name run {}
                   |            ^^^
                "#],
    );
}
