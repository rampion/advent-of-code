use std::str::FromStr;

struct Length { length: usize };

use std::iter::FromIterator;
impl<A> FromIterator<A> for Length {
    fn from_iter<S>(src: S) -> Self
    where
        S: IntoIterator<Item = A>,
    {
        let mut length: usize = 0;
        for _ in src {
            length += 1;
        }
        Length{length}
    }
}

#[derive(Clone)]
struct Password(String);

#[derive(Clone)]
struct Rule {
    low: usize,
    high: usize,
    key: char,
}

#[derive(Clone)]
struct InputLine {
    rule: Rule,
    password: Password,
}

impl InputLine {
    fn old_validate(&self) -> bool {
        let Length{length} = self
            .password
            .0
            .chars()
            .filter(|c| *c == self.rule.key)
            .collect();
        self.rule.low <= length && length <= self.rule.high
    }

    fn new_validate(&self) -> bool {
        let chars: Vec<char> = self.password.0.chars().collect();
        (chars[self.rule.low - 1] == self.rule.key) ^ (chars[self.rule.high - 1] == self.rule.key)
    }
}

impl FromStr for InputLine {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (rule, password) = match &s.split(": ").collect::<Vec<_>>()[..] {
            [rule, password] => Ok((*rule, String::from(*password))),
            _ => Err(Error::from(format!(
                "Unable to locate rule and password in {}",
                s
            ))),
        }?;

        let (bounds, key) = match &rule.split(" ").collect::<Vec<_>>()[..] {
            [bounds, key] if key.len() == 1 => Ok((*bounds, key.chars().nth(0).unwrap())),
            _ => Err(Error::from(format!(
                "Unable to locate bounds and key in {}",
                rule
            ))),
        }?;

        let (&low, &high) = match &(bounds
            .split("-")
            .map(|s| s.parse().map_err(Error::from))
            .collect::<Result<Vec<usize>, Error>>()?)[..]
        {
            [low, high] => Ok((low, high)),
            _ => Err(format!("Incorrect number of bounds in {}", bounds)),
        }?;

        Ok(InputLine {
            rule: Rule { low, high, key },
            password: Password(password),
        })
    }
}

use std::path::PathBuf;
use structopt::StructOpt;
#[derive(StructOpt)]
#[structopt(
    name = "solve",
    about = "solve the puzzles from the Advent of Code 2020, day 2"
)]
struct Opt {
    /// path to input file; should contain rule/password lines
    input: PathBuf,
}

use std::error;
type Error = Box<dyn error::Error>;

use std::fs::File;
use std::io::{BufRead, BufReader};
fn main() -> Result<(), Error> {
    let opts = Opt::from_args();

    let file = File::open(opts.input.clone())?;

    let input = BufReader::new(file)
        .lines()
        .map(|read| read?.parse().map_err(Error::from))
        .collect::<Result<Vec<InputLine>, Error>>()?;

    println!("passwords in {:?} satisfying old rules", opts.input);
    println!(
        "{}",
        input
            .iter()
            .cloned()
            .filter(InputLine::old_validate)
            .collect::<Length>()
            .0
    );

    println!("passwords in {:?} satisfying new rules", opts.input);
    println!(
        "{}",
        input
            .iter()
            .cloned()
            .filter(InputLine::new_validate)
            .collect::<Length>()
            .0
    );

    Ok(())
}
