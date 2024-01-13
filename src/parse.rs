use crate::flag::Flag;
use nom::bytes::complete::{tag, take_while1};
use nom::character::complete::multispace0;
use nom::multi::{fold_many1, separated_list1};
use nom::sequence::{delimited, preceded};
use nom::{IResult, Parser};
use std::backtrace::Backtrace;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("missing {0}")]
    MissingError(String),
    #[error("multiple {0}")]
    MultipleError(String),
    #[error("no $names")]
    NomError {
        #[from]
        source: nom::Err<nom::error::Error<String>>,
        backtrace: Backtrace,
    },
}

type Result<T> = std::result::Result<T, ParseError>;

impl From<nom::Err<nom::error::Error<&str>>> for ParseError {
    fn from(err: nom::Err<nom::error::Error<&str>>) -> Self {
        match err {
            nom::Err::Error(err) => Self::NomError {
                source: nom::Err::Error(nom::error::ParseError::from_error_kind(
                    err.input.to_string(),
                    err.code,
                )),
                backtrace: Backtrace::capture(),
            },
            nom::Err::Failure(err) => Self::NomError {
                source: nom::Err::Failure(nom::error::ParseError::from_error_kind(
                    err.input.to_string(),
                    err.code,
                )),
                backtrace: Backtrace::capture(),
            },
            nom::Err::Incomplete(_) => Self::NomError {
                source: err.to_owned(),
                backtrace: Backtrace::capture(),
            },
        }
    }
}

fn string_chars(input: &str) -> IResult<&str, &str> {
    take_while1(|c| c != '"').parse(input)
}

fn string(input: &str) -> IResult<&str, String> {
    let (input, _) = multispace0.parse(input)?;
    let (input, _) = tag("\"").parse(input)?;
    let (input, string) = string_chars.parse(input)?;
    let (input, _) = tag("\"").parse(input)?;
    Ok((input, string.to_string()))
}

fn strings(input: &str) -> IResult<&str, Vec<String>> {
    separated_list1(preceded(multispace0, tag(",")), string).parse(input)
}

fn strings_list(input: &str) -> IResult<&str, Vec<String>> {
    delimited(
        preceded(multispace0, tag("[")),
        strings,
        preceded(multispace0, tag("]")),
    )
    .parse(input)
}

#[derive(Debug)]
pub enum Element {
    Names(Vec<String>),
}

fn names(input: &str) -> IResult<&str, Element> {
    let (input, names) = preceded(
        tag("names"),
        preceded(preceded(multispace0, tag("=")), strings_list),
    )(input)?;
    Ok((input, Element::Names(names)))
}

fn variable(input: &str) -> IResult<&str, Element> {
    let (input, variable) = preceded(multispace0, preceded(tag("$"), names))(input)?;
    Ok((input, variable))
}

fn elements(input: &str) -> IResult<&str, Vec<Element>> {
    fold_many1(variable, Vec::new, |mut elements, element| {
        elements.push(element);
        elements
    })(input)
}

pub fn parse_flag(input: &str) -> Result<Flag> {
    let (_, elements) = elements(input)?;
    let mut names = None;
    for element in elements {
        match element {
            Element::Names(_names) => {
                if let None = names {
                    names.replace(_names);
                } else {
                    return Err(ParseError::MultipleError("$names".to_string()));
                }
            }
        }
    }
    Ok(Flag {
        names: names.ok_or(ParseError::MissingError("$names".to_string()))?,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::{ErrorKind, ParseError};

    #[test]
    fn test_string_chars() {
        assert_eq!(
            string_chars(""),
            Err(nom::Err::Error(ParseError::from_error_kind(
                "",
                ErrorKind::TakeWhile1
            )))
        );
        assert_eq!(
            string_chars("\""),
            Err(nom::Err::Error(ParseError::from_error_kind(
                "\"",
                ErrorKind::TakeWhile1
            )))
        );
        assert_eq!(string_chars("a\""), Ok(("\"", "a")));
        assert_eq!(string_chars("foo"), Ok(("", "foo")));
        assert_eq!(string_chars("foo bar"), Ok(("", "foo bar")));
        assert_eq!(string_chars("foo\""), Ok(("\"", "foo")));
        assert_eq!(string_chars("foo \""), Ok(("\"", "foo ")));
        assert_eq!(string_chars("foo bar\""), Ok(("\"", "foo bar")));
        assert_eq!(string_chars("foo\"baz"), Ok(("\"baz", "foo")));
        assert_eq!(string_chars("foo \"baz"), Ok(("\"baz", "foo ")));
        assert_eq!(string_chars("foo bar\"baz"), Ok(("\"baz", "foo bar")));
    }

    #[test]
    fn test_string() {
        assert_eq!(
            string("\"\""),
            Err(nom::Err::Error(ParseError::from_error_kind(
                "\"",
                ErrorKind::TakeWhile1
            )))
        );
        assert_eq!(string("\"foo\""), Ok(("", "foo".to_string())));
        assert_eq!(string(" \"foo\""), Ok(("", "foo".to_string())));
        assert_eq!(string("\t\"foo\""), Ok(("", "foo".to_string())));
        assert_eq!(string("\t \"foo\""), Ok(("", "foo".to_string())));
        assert_eq!(string("\"foo\""), Ok(("", "foo".to_string())));
        assert_eq!(
            string(""),
            Err(nom::Err::Error(ParseError::from_error_kind(
                "",
                ErrorKind::Tag
            )))
        );
        assert_eq!(
            string("\""),
            Err(nom::Err::Error(ParseError::from_error_kind(
                "",
                ErrorKind::TakeWhile1
            )))
        );
        assert_eq!(
            string("\"foo"),
            Err(nom::Err::Error(ParseError::from_error_kind(
                "",
                ErrorKind::Tag
            )))
        );
        assert_eq!(
            string("foo\""),
            Err(nom::Err::Error(ParseError::from_error_kind(
                "foo\"",
                ErrorKind::Tag
            )))
        );
    }

    #[test]
    fn test_strings() {
        assert_eq!(strings("\"foo\""), Ok(("", vec!["foo".to_string()])));
        assert_eq!(strings(" \"foo\""), Ok(("", vec!["foo".to_string()])));
        assert_eq!(strings("\t\"foo\""), Ok(("", vec!["foo".to_string()])));
        assert_eq!(strings("\n\"foo\""), Ok(("", vec!["foo".to_string()])));
        assert_eq!(strings("\r\"foo\""), Ok(("", vec!["foo".to_string()])));
        assert_eq!(strings("\r\n\"foo\""), Ok(("", vec!["foo".to_string()])));
        assert_eq!(strings("\"foo\","), Ok((",", vec!["foo".to_string()])));
        assert_eq!(
            strings("\"foo\",\"bar\""),
            Ok(("", vec!["foo".to_string(), "bar".to_string()]))
        );
        assert_eq!(
            strings("\"foo\", \"bar\""),
            Ok(("", vec!["foo".to_string(), "bar".to_string()]))
        );
        assert_eq!(
            strings("\"foo\",\t\"bar\""),
            Ok(("", vec!["foo".to_string(), "bar".to_string()]))
        );
        assert_eq!(
            strings("\"foo\",\n\"bar\""),
            Ok(("", vec!["foo".to_string(), "bar".to_string()]))
        );
        assert_eq!(
            strings("\"foo\",\r\"bar\""),
            Ok(("", vec!["foo".to_string(), "bar".to_string()]))
        );
        assert_eq!(
            strings("\"foo\",\r\n\"bar\""),
            Ok(("", vec!["foo".to_string(), "bar".to_string()]))
        );
    }

    #[test]
    fn test_strings_list() {
        assert_eq!(strings_list("[\"foo\"]"), Ok(("", vec!["foo".to_string()])));
        assert_eq!(
            strings_list("\n[\"foo\"]"),
            Ok(("", vec!["foo".to_string()]))
        );
        assert_eq!(
            strings_list("[\"foo\"\n, \t\"bar\"]"),
            Ok(("", vec!["foo".to_string(), "bar".to_string()]))
        );
    }
}
