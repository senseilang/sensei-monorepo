use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token<'src> {
    Ident(&'src str),
    Num(&'src str),

    RoundOpen,
    RoundClose,
    Percent,

    True,
    False,
}

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    source: &'src str,
    chars: std::iter::Peekable<std::str::CharIndices<'src>>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
        }
    }

    fn current(&mut self) -> usize {
        self.chars.peek().map_or(self.source.len(), |(i, _)| *i)
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = (Token<'src>, Span<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((start, c)) = self.chars.next() {
            match c {
                '-' | '0'..='9' => {
                    while self.chars.next_if(|(_, c)| c.is_ascii_digit()).is_some() {}
                    let span = Span::new(start, self.current());
                    return Some((Token::Num(&self.source[span.range()]), span));
                }
                '_' | 'a'..='z' | 'A'..='Z' => {
                    while self
                        .chars
                        .next_if(|(_, c)| c.is_ascii_alphanumeric() || *c == '_')
                        .is_some()
                    {}
                    let span = Span::new(start, self.current());
                    let name = &self.source[span.range()];
                    return Some((
                        match name {
                            "true" => Token::True,
                            "false" => Token::False,
                            _ => Token::Ident(name),
                        },
                        span,
                    ));
                }
                '(' => return Some((Token::RoundOpen, (start..start + 1).into())),
                ')' => return Some((Token::RoundClose, (start..start + 1).into())),
                _ => {}
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let tokens: Vec<_> = Lexer::new(
            r#"
            hello nice

            34279

            (-123 aaa) )) a_3473bob

            void true

            %
            false
            "#,
        )
        .collect();
        use Token::*;
        assert_eq!(
            tokens.into_iter().map(|(t, _)| t).collect::<Vec<_>>(),
            [
                Ident("hello"),
                Ident("nice"),
                Num("34279"),
                RoundOpen,
                Num("-123"),
                Ident("aaa"),
                RoundClose,
                RoundClose,
                RoundClose,
                Ident("a_3473bob"),
                Ident("void"),
                True,
                Percent,
                False
            ]
        );
    }
}
