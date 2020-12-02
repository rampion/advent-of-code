use std::collections::HashSet;
use std::error;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(
    name = "solve",
    about = "solve the puzzles from the Advent of Code 2020, day 1"
)]
struct Opt {
    #[structopt(short, long, default_value = "2020")]
    year: i32,

    /// path to input file; should contain integers, one per line
    input: PathBuf,
}

type Error = Box<dyn error::Error>;

fn main() -> Result<(), Error> {
    let opts = Opt::from_args();

    let file = File::open(opts.input.clone())?;

    let input = BufReader::new(file)
        .lines()
        .map(|read| read?.parse().map_err(Error::from))
        .collect::<Result<Vec<i32>, Error>>()?;

    println!("part one:");
    if let Some((a, b)) = part_one(&input, opts.year) {
        println!("{} * {} = {}", a, b, a * b);
    } else {
        eprintln!(
            "No pair of numbers adding to {} exists in {:?}",
            opts.year,
            opts.input.as_os_str()
        );
    }

    println!("part two:");
    if let Some((a, b, c)) = part_two(&input, opts.year) {
        println!("{} * {} * {} = {}", a, b, c, a * b * c);
    } else {
        eprintln!(
            "No triple of numbers adding to {} exists in {:?}",
            opts.year,
            opts.input.as_os_str()
        );
    }

    Ok(())
}

fn part_one(input: &[i32], total: i32) -> Option<(i32, i32)> {
    let input_set: HashSet<i32> = input.iter().cloned().collect();

    for &a in input.iter() {
        let b = total - a;
        if input_set.contains(&b) {
            return Some((a, b));
        }
    }
    return None;
}

fn part_two(input: &[i32], total: i32) -> Option<(i32, i32, i32)> {
    let input_set: HashSet<i32> = input.iter().cloned().collect();

    for (ix, &a) in input.iter().enumerate() {
        for &b in input[ix + 1..].iter() {
            let c = total - a - b;
            if input_set.contains(&c) {
                return Some((a, b, c));
            }
        }
    }
    return None;
}
