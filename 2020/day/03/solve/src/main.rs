use std::error;
use std::fs;
use std::iter::FromIterator;
use std::ops::Index;
use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

type Error = Box<dyn error::Error>;

#[derive(StructOpt)]
#[structopt(
    name = "solve",
    about = "solve the puzzles from the Advent of Code 2020, day 3"
)]
struct Opt {
    /// path to input file; should contain mountain map
    input: PathBuf,
}
fn main() -> Result<(), Error> {
    let opts = Opt::from_args();
    let input: TreeLocations = fs::read_to_string(opts.input.clone())?.parse()?;

    println!("part one: number of collisions for the slope of right 3, down 1");
    println!("{}", input.collisions((3, 1)));

    let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)];
    println!("part two: product of collisions for slopes {:?}", slopes);
    println!(
        "{}",
        slopes
            .iter()
            .map(|&slope| input.collisions(slope))
            .product::<usize>()
    );
    Ok(())
}

struct TreeLocations {
    rows: Vec<Vec<bool>>,
}

impl TreeLocations {
    fn collisions(&self, (dx, dy): (usize, usize)) -> usize {
        (0..)
            .step_by(dx)
            .zip((0..self.height()).step_by(dy))
            .filter(|&p| self[p])
            .collect::<Length>()
            .length
    }

    fn height(&self) -> usize {
        self.rows.len()
    }
}

impl Index<(usize, usize)> for TreeLocations {
    type Output = bool;
    fn index(&self, (x, y): (usize, usize)) -> &bool {
        &self.rows[y][x % self.rows[y].len()]
    }
}

impl FromStr for TreeLocations {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(TreeLocations {
            rows: s
                .lines()
                .map(|line| {
                    line.chars()
                        .map(|ch| match ch {
                            '#' => Ok(true),
                            '.' => Ok(false),
                            _ => Err(Error::from(format!(
                                "illegal map character {:?}, expected '#' or '.'",
                                ch
                            ))),
                        })
                        .collect::<Result<Vec<bool>, Error>>()
                })
                .collect::<Result<Vec<Vec<bool>>, Error>>()?,
        })
    }
}

struct Length {
    length: usize,
}

impl<A> FromIterator<A> for Length {
    fn from_iter<S>(s: S) -> Self
    where
        S: IntoIterator<Item = A>,
    {
        let mut length = 0;
        for _ in s {
            length += 1;
        }
        Length { length }
    }
}
