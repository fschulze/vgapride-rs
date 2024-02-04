use crate::flag::{Command, Commands, Flag};
use ahash::AHashMap;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag, take_while1, take_while_m_n};
use nom::character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, space1};
use nom::combinator::{all_consuming, map, map_res, opt, recognize};
use nom::multi::{fold_many1, many0_count, separated_list1};
use nom::sequence::{delimited, pair, preceded, separated_pair, terminated, tuple};
use nom::{IResult, Parser};
use std::backtrace::Backtrace;
use std::str::FromStr;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("missing {0}")]
    MissingError(String),
    #[error("multiple {0}")]
    MultipleError(String),
    #[error("unknown polygon type")]
    UnknownPolygonType,
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

pub fn name(input: &str) -> IResult<&str, &str> {
    recognize(pair(alpha1, many0_count(alt((alphanumeric1, tag("_")))))).parse(input)
}

fn variable_name(input: &str) -> IResult<&str, &str> {
    preceded(multispace0, preceded(tag("$"), name)).parse(input)
}

fn string_chars(input: &str) -> IResult<&str, &str> {
    take_while1(|c| c != '"').parse(input)
}

fn string(input: &str) -> IResult<&str, String> {
    map(
        delimited(preceded(multispace0, tag("\"")), string_chars, tag("\"")),
        |r: &str| r.to_string(),
    )
    .parse(input)
}

fn comma(input: &str) -> IResult<&str, &str> {
    preceded(multispace0, tag(",")).parse(input)
}

fn strings(input: &str) -> IResult<&str, Vec<String>> {
    separated_list1(comma, string).parse(input)
}

fn strings_list(input: &str) -> IResult<&str, Vec<String>> {
    delimited(
        preceded(multispace0, tag("[")),
        strings,
        preceded(multispace0, tag("]")),
    )
    .parse(input)
}

fn signed_num(input: &str) -> IResult<&str, (bool, &str)> {
    let (input, sign) = opt(alt((char('+'), char('-'))))(input)?;
    let (input, num) = digit1(input)?;
    let neg = match sign {
        Some(sign) => sign == '-',
        None => false,
    };
    Ok((input, (neg, num)))
}

fn unsigned8_from(input: &str) -> std::result::Result<u8, std::num::ParseIntError> {
    u8::from_str(input)
}

fn signed16_from(input: &str) -> std::result::Result<i16, std::num::ParseIntError> {
    i16::from_str(input)
}

fn unsigned8(input: &str) -> IResult<&str, u8> {
    map_res(preceded(multispace0, digit1), unsigned8_from).parse(input)
}

fn integer16(input: &str) -> IResult<&str, i16> {
    map_res(preceded(multispace0, recognize(signed_num)), signed16_from).parse(input)
}

#[derive(Debug)]
pub enum Color {
    RGBColor(crate::flag::Color),
    Variable(String),
}

fn unsigned8_from_hex(input: &str) -> std::result::Result<u8, std::num::ParseIntError> {
    u8::from_str_radix(input, 16)
}

fn is_hex_digit(c: char) -> bool {
    c.is_digit(16)
}

fn hex_primary(input: &str) -> IResult<&str, u8> {
    map_res(take_while_m_n(2, 2, is_hex_digit), unsigned8_from_hex)(input)
}

fn color_hex(input: &str) -> IResult<&str, Color> {
    map(
        preceded(
            preceded(multispace0, tag("#")),
            tuple((hex_primary, hex_primary, hex_primary)),
        ),
        |(r, g, b)| Color::RGBColor(crate::flag::Color { r, g, b }),
    )
    .parse(input)
}

fn color_rgb(input: &str) -> IResult<&str, Color> {
    map(
        preceded(
            multispace0,
            preceded(
                tag("RGB"),
                delimited(
                    tag("("),
                    tuple((
                        terminated(unsigned8, comma),
                        terminated(unsigned8, comma),
                        unsigned8,
                    )),
                    tag(")"),
                ),
            ),
        ),
        |(r, g, b)| Color::RGBColor(crate::flag::Color { r, g, b }),
    )
    .parse(input)
}

fn color_variable(input: &str) -> IResult<&str, Color> {
    map(variable_name, |r: &str| Color::Variable(r.to_string())).parse(input)
}

fn color(input: &str) -> IResult<&str, Color> {
    alt((color_hex, color_rgb, color_variable)).parse(input)
}

fn colors(input: &str) -> IResult<&str, Vec<Color>> {
    fold_many1(color, Vec::new, |mut colors, color| {
        colors.push(color);
        colors
    })
    .parse(input)
}

#[derive(Debug)]
pub enum StructureType {
    Horizontal,
    Vertical,
}

#[derive(Debug)]
pub enum Element {
    Comment,
    Names(Vec<String>),
    Description(String),
    Credits(String),
    TextSize(i16),
    TextColor(Color),
    ColorDeclaration(String, Color),
    Include(String),
    Structure(StructureType, Vec<Color>),
    Polygon(
        crate::flag::PolygonType,
        crate::flag::Point,
        Vec<crate::flag::Point>,
        Color,
    ),
}

fn comment(input: &str) -> IResult<&str, Element> {
    map(
        preceded(multispace0, pair(tag("//"), is_not("\n\r"))),
        |_| Element::Comment,
    )
    .parse(input)
}

fn names(input: &str) -> IResult<&str, Element> {
    let (input, names) = preceded(
        tag("names"),
        preceded(preceded(multispace0, tag("=")), strings_list),
    )(input)?;
    Ok((input, Element::Names(names)))
}

fn description(input: &str) -> IResult<&str, Element> {
    let (input, description) = preceded(
        tag("description"),
        preceded(preceded(multispace0, tag("=")), string),
    )(input)?;
    Ok((input, Element::Description(description)))
}

fn credits(input: &str) -> IResult<&str, Element> {
    let (input, credits) = preceded(
        tag("credits"),
        preceded(preceded(multispace0, tag("=")), string),
    )(input)?;
    Ok((input, Element::Credits(credits)))
}

fn textsize(input: &str) -> IResult<&str, Element> {
    let (input, textsize) = preceded(
        tag("textsize"),
        preceded(preceded(multispace0, tag("=")), integer16),
    )(input)?;
    Ok((input, Element::TextSize(textsize)))
}

fn textcolor(input: &str) -> IResult<&str, Element> {
    let (input, textcolor) = preceded(
        tag("textcolor"),
        preceded(preceded(multispace0, tag("=")), color),
    )(input)?;
    Ok((input, Element::TextColor(textcolor)))
}

fn color_declaration(input: &str) -> IResult<&str, Element> {
    let (input, (name, color)) =
        separated_pair(variable_name, preceded(multispace0, tag("=")), color)(input)?;
    Ok((input, Element::ColorDeclaration(name.to_string(), color)))
}

fn variable_declaration(input: &str) -> IResult<&str, Element> {
    let (input, variable_declaration) = preceded(
        multispace0,
        preceded(
            tag("$"),
            alt((names, description, credits, textsize, textcolor)),
        ),
    )(input)?;
    Ok((input, variable_declaration))
}

fn include(input: &str) -> IResult<&str, Element> {
    map(
        preceded(
            multispace0,
            preceded(terminated(tag("#include"), space1), string),
        ),
        |name| Element::Include(name),
    )
    .parse(input)
}

fn structure(input: &str) -> IResult<&str, Element> {
    map(
        pair(
            preceded(multispace0, alt((tag("horizontal"), tag("vertical")))),
            delimited(
                preceded(multispace0, tag("{")),
                colors,
                preceded(multispace0, tag("}")),
            ),
        ),
        |(kind, colors)| {
            Element::Structure(
                match kind {
                    "horizontal" => StructureType::Horizontal,
                    "vertical" => StructureType::Vertical,
                    _ => panic!("how did we get here?"),
                },
                colors,
            )
        },
    )
    .parse(input)
}

fn point(input: &str) -> IResult<&str, crate::flag::Point> {
    map(
        preceded(
            multispace0,
            delimited(
                tag("("),
                pair(terminated(integer16, comma), integer16),
                preceded(multispace0, tag(")")),
            ),
        ),
        |(x, y)| crate::flag::Point { x, y },
    )
    .parse(input)
}

fn points(input: &str) -> IResult<&str, Vec<crate::flag::Point>> {
    separated_list1(comma, point).parse(input)
}

fn points_list(input: &str) -> IResult<&str, Vec<crate::flag::Point>> {
    delimited(
        preceded(multispace0, tag("[")),
        points,
        preceded(multispace0, tag("]")),
    )
    .parse(input)
}

fn polygon_type(input: &str) -> IResult<&str, crate::flag::PolygonType> {
    let (input, kind) = preceded(multispace0, tag("Filled")).parse(input)?;
    match kind {
        "Filled" => Ok((input, crate::flag::PolygonType::Filled)),
        _ => panic!("how did we reach this?"),
    }
}

fn polygon(input: &str) -> IResult<&str, Element> {
    map(
        preceded(
            multispace0,
            preceded(
                tag("Polygon"),
                tuple((
                    terminated(polygon_type, comma),
                    terminated(point, comma),
                    terminated(points_list, comma),
                    color,
                )),
            ),
        ),
        |(kind, start, points, color)| Element::Polygon(kind, start, points, color),
    )
    .parse(input)
}

fn command(input: &str) -> IResult<&str, Element> {
    alt((include, polygon)).parse(input)
}

fn element(input: &str) -> IResult<&str, Element> {
    alt((
        comment,
        variable_declaration,
        color_declaration,
        structure,
        command,
    ))
    .parse(input)
}

fn elements(input: &str) -> IResult<&str, Vec<Element>> {
    fold_many1(element, Vec::new, |mut elements, element| {
        elements.push(element);
        elements
    })(input)
}

pub fn parse_flag(input: &str) -> Result<(Vec<String>, Flag)> {
    let (_, elements) = all_consuming(terminated(elements, multispace0))(input)?;
    let mut names = None;
    let mut description = None;
    let mut credits = None;
    let mut textsize = None;
    let mut textcolor = None;
    let mut commands = Commands::new();
    let mut includes = Vec::new();
    let mut colors: AHashMap<String, crate::flag::Color> = Default::default();
    colors.insert("black".to_string(), crate::flag::Color::new(0, 0, 0));
    colors.insert("white".to_string(), crate::flag::Color::new(255, 255, 255));
    for element in elements {
        match element {
            Element::Names(_names) => {
                if let None = names {
                    names.replace(_names);
                } else {
                    return Err(ParseError::MultipleError("$names".to_string()));
                }
            }
            Element::Description(_description) => {
                if let None = description {
                    description.replace(_description);
                } else {
                    return Err(ParseError::MultipleError("$description".to_string()));
                }
            }
            Element::Credits(_credits) => {
                if let None = credits {
                    credits.replace(_credits);
                } else {
                    return Err(ParseError::MultipleError("$credits".to_string()));
                }
            }
            Element::TextSize(_textsize) => {
                if let None = textsize {
                    textsize.replace(_textsize);
                } else {
                    return Err(ParseError::MultipleError("$textsize".to_string()));
                }
            }
            Element::TextColor(_textcolor) => {
                if let None = textcolor {
                    textcolor.replace(match _textcolor {
                        Color::RGBColor(color) => color,
                        Color::Variable(_name) => *colors
                            .get(&_name)
                            .ok_or(ParseError::MissingError(format!("${}", _name)))?,
                    });
                } else {
                    return Err(ParseError::MultipleError("$textcolor".to_string()));
                }
            }
            Element::ColorDeclaration(name, color) => {
                if colors.contains_key(&name) {
                    return Err(ParseError::MultipleError(format!("${}", name)));
                }
                match color {
                    Color::RGBColor(color) => {
                        colors.insert(name, color);
                    }
                    Color::Variable(_name) => {
                        let color = colors
                            .get(&_name)
                            .ok_or(ParseError::MissingError(format!("${}", _name)))?;
                        colors.insert(_name, *color);
                    }
                };
            }
            Element::Include(name) => {
                includes.push(name);
            }
            Element::Structure(kind, _colors) => {
                let pixels = match kind {
                    StructureType::Horizontal => crate::flag::WIDTH - 1,
                    StructureType::Vertical => crate::flag::HEIGHT - 1,
                };
                let stripes =
                    i16::try_from(_colors.len()).expect("couldn't convert number of colors to i16");
                for (index, _color) in _colors.iter().enumerate() {
                    let index = i16::try_from(index).expect("couldn't convert colors index to i16");
                    let color = match _color {
                        Color::RGBColor(color) => color,
                        Color::Variable(_name) => colors
                            .get(_name)
                            .ok_or(ParseError::MissingError(format!("${}", _name)))?,
                    };
                    let low = (pixels * index) / stripes;
                    let high = (pixels * (index + 1)) / stripes;
                    let (a, b) = match kind {
                        StructureType::Horizontal => (
                            crate::flag::Point { x: low, y: 0 },
                            crate::flag::Point {
                                x: high,
                                y: crate::flag::HEIGHT,
                            },
                        ),
                        StructureType::Vertical => (
                            crate::flag::Point { x: 0, y: low },
                            crate::flag::Point {
                                x: crate::flag::WIDTH,
                                y: high,
                            },
                        ),
                    };
                    commands.push(Command::Rectangle(a, b, *color));
                }
            }
            Element::Polygon(kind, start, points, _color) => {
                let color = match _color {
                    Color::RGBColor(color) => color,
                    Color::Variable(_name) => *colors
                        .get(&_name)
                        .ok_or(ParseError::MissingError(format!("${}", _name)))?,
                };
                commands.push(Command::Polygon(kind, start, points, color));
            }
            Element::Comment => {}
        }
    }
    Ok((
        includes,
        Flag {
            names: names.ok_or(ParseError::MissingError("$names".to_string()))?,
            description: description.ok_or(ParseError::MissingError("$description".to_string()))?,
            credits,
            textsize: textsize.unwrap_or(4),
            textcolor: textcolor.unwrap_or(crate::flag::Color::new(255, 255, 255)),
            commands,
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::error::Error;
    use nom::error::FromExternalError;
    use nom::error::{ErrorKind, ParseError};

    #[test]
    fn test_integer16() {
        assert_eq!(integer16("32767"), Ok(("", 32767)));
        assert_eq!(integer16("-32768"), Ok(("", -32768)));
        assert_eq!(
            integer16("32768"),
            Err(nom::Err::Error(Error::from_external_error(
                "32768",
                ErrorKind::MapRes,
                "32768".parse::<i16>().unwrap_err()
            )))
        );
        assert_eq!(
            integer16("-32769"),
            Err(nom::Err::Error(Error::from_external_error(
                "-32769",
                ErrorKind::MapRes,
                "-32769".parse::<i16>().unwrap_err()
            )))
        );
    }

    #[test]
    fn test_name() {
        assert_eq!(name("a"), Ok(("", "a")));
        assert_eq!(name("a1"), Ok(("", "a1")));
        assert_eq!(name("ab"), Ok(("", "ab")));
        assert_eq!(
            name("1a"),
            Err(nom::Err::Error(ParseError::from_error_kind(
                "1a",
                ErrorKind::Alpha
            )))
        );
        assert_eq!(name("a_"), Ok(("", "a_")));
        assert_eq!(name("a_1"), Ok(("", "a_1")));
        assert_eq!(name("a_b"), Ok(("", "a_b")));
        assert_eq!(
            name("_1a"),
            Err(nom::Err::Error(ParseError::from_error_kind(
                "_1a",
                ErrorKind::Alpha
            )))
        );
    }

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
