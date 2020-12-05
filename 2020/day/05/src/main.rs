use std::error;
use std::fs;
use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

type Error = Box<dyn error::Error>;

#[derive(StructOpt)]
#[structopt(
    name = "solve",
    about = "solve the puzzles from the Advent of Code 2020, day 5"
)]
struct Opt {
    /// path to input file; should contain list of boarding passes
    input: PathBuf,
}
fn main() -> Result<(), Error> {
    let opts = Opt::from_args();

    let boarding_pass_ids = fs::read_to_string(opts.input.clone())?
        .lines()
        .map(|s| BoardingPass::from_str(s).map(|b| b.id()))
        .collect::<Result<Vec<u16>, Error>>()?;

    println!("part 1: highest seat id on a boarding pass");
    println!("{:?}", boarding_pass_ids.iter().max());

    println!("part 2: ID of seat missing from list");
    println!("{:?}", find_missing_id(boarding_pass_ids));

    Ok(())
}

fn find_missing_id(mut ids: Vec<u16>) -> Option<u16> {
    ids.sort();
    for ix in 1..ids.len() {
        let prev_id = ids[ix] - 1;
        if prev_id != ids[ix - 1] {
            return Some(prev_id);
        }
    }
    None
}

struct BoardingPass {
    row: u8,
    column: u8,
}

impl BoardingPass {
    fn id(&self) -> u16 {
        (self.row as u16) * 8u16 + (self.column as u16)
    }
}

impl FromStr for BoardingPass {
    type Err = Error;
    fn from_str(s: &str) -> Result<BoardingPass, Error> {
        if s.len() != 10 {
            return Err(Error::from(format!(
                "Illegal boarding pass {:?}, expected 10 bytes, found {}",
                s,
                s.len()
            )));
        }

        let row = s[0..7]
            .chars()
            .map(|c| match c {
                'F' => Ok('0'),
                'B' => Ok('1'),
                _ => Err(Error::from(format!(
                    "Illegal character {:?} in row of boarding pass {:?}",
                    c, s
                ))),
            })
            .collect::<Result<String, Error>>()
            .and_then(|bs| u8::from_str_radix(&bs, 2).map_err(Error::from))?;

        let column = s[7..10]
            .chars()
            .map(|c| match c {
                'L' => Ok('0'),
                'R' => Ok('1'),
                _ => Err(Error::from(format!(
                    "Illegal character {:?} in column of boarding pass {:?}",
                    c, s
                ))),
            })
            .collect::<Result<String, Error>>()
            .and_then(|bs| u8::from_str_radix(&bs, 2).map_err(Error::from))?;

        Ok(BoardingPass { row, column })
    }
}

//use std::fmt::Display;
//impl Display for BoardingPass {}
