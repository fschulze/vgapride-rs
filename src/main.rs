#![feature(error_generic_member_access)]
use ahash::AHashMap;
use anyhow::Context;
use clap::Parser;
use concolor_clap::ColorChoice;
use env_logger::WriteStyle;
use std::path::PathBuf;
use thiserror::Error;
use vgapride::flag::{Commands, Flag};
use vgapride::svg::render;

#[derive(Debug, Error)]
pub enum MainError {
    #[error(transparent)]
    AnyhowError(#[from] anyhow::Error),
    #[error(transparent)]
    IOError(#[from] std::io::Error),
    #[error(transparent)]
    ParseError(
        #[backtrace]
        #[from]
        vgapride::parse::ParseError,
    ),
}

type Result<T> = std::result::Result<T, MainError>;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[clap(color = concolor_clap::color_choice())]
struct Args {
    /// path to flag file or directory with flag files
    path: PathBuf,
    #[command(flatten)]
    color: concolor_clap::Color,
    #[command(flatten)]
    verbose: clap_verbosity_flag::Verbosity,
}

fn read_flag(path: PathBuf) -> Result<Option<(Vec<String>, Flag)>> {
    if let Some(extension) = path.extension() {
        let extension = extension.to_ascii_lowercase();
        if extension == "flag" && path.is_file() {
            let content = std::fs::read_to_string(path.clone())?;
            return Ok(Some(
                vgapride::parse::parse_flag(&content)
                    .with_context(|| format!("Error reading {:?}", path))?,
            ));
        }
    }
    Ok(None)
}

fn run(args: Args) -> Result<()> {
    let mut flags = Vec::new();
    if args.path.is_dir() {
        for entry in args.path.read_dir()? {
            if let Ok(entry) = entry {
                if let Some(flag) = read_flag(entry.path())? {
                    flags.push(flag);
                }
            }
        }
    } else {
        if let Some(flag) = read_flag(args.path)? {
            flags.push(flag);
        }
    }
    let mut name2index: AHashMap<String, usize> = AHashMap::new();
    for (index, (_includes, flag)) in flags.iter().enumerate() {
        for name in &flag.names {
            name2index.insert(name.to_string(), index);
        }
    }
    for (includes, flag) in &flags {
        let mut commands = Commands::new();
        for include in includes {
            let (_, flag) = &flags[name2index[include]];
            commands.extend(&flag.commands);
        }
        commands.extend(&flag.commands);
        println!("{:?}", flag.names);
        println!("includes {:?}", includes);
        let mut f = std::fs::File::create(format!("{}.svg", flag.names[0]))?;
        render(&mut f, commands)?;
    }
    Ok(())
}

fn main() -> Result<()> {
    let args = Args::parse();
    env_logger::Builder::new()
        .filter_level(args.verbose.log_level_filter())
        .write_style(match args.color.color {
            ColorChoice::Always => WriteStyle::Always,
            ColorChoice::Auto => WriteStyle::Auto,
            ColorChoice::Never => WriteStyle::Never,
        })
        .init();
    log::debug!("{:?}", args);
    run(args)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_cli() {
        use clap::CommandFactory;
        Args::command().debug_assert()
    }
}
