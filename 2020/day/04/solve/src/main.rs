#[macro_use]
extern crate lazy_static;

use regex::Regex;
use std::collections::HashMap;
use std::error;
use std::fs;
use std::path::PathBuf;
use std::str::FromStr;
use structopt::StructOpt;

type Error = Box<dyn error::Error>;

#[derive(StructOpt)]
#[structopt(
    name = "solve",
    about = "solve the puzzles from the Advent of Code 2020, day 4"
)]
struct Opt {
    /// path to input file; should contain list of passports
    input: PathBuf,
}
fn main() -> Result<(), Error> {
    let opts = Opt::from_args();

    let passports = fs::read_to_string(opts.input.clone())?
        .split("\n\n")
        .map(Passport::from_str)
        .collect::<Result<Vec<Passport>, Error>>()?;
    let num_passports = passports.len();

    let populated = contains_fields(
        &PASSPORT_FIELDS
            .iter()
            .cloned()
            .filter(|&f| f != "cid")
            .collect::<Vec<&str>>()[..],
        passports,
    );
    let num_populated = populated.len();

    println!("part 1: passports containing all fields except possibly cid");
    println!("{:?}: {} / {}", opts.input, num_populated, num_passports);

    let validated = validate_fields(&PASSPORT_PREDICATES, populated);
    let num_validated = validated.len();

    println!("part 2: passports with defined and valid values for all fields except possibly cid");
    println!("{:?}: {} / {}", opts.input, num_validated, num_passports);

    Ok(())
}

struct Passport {
    values: HashMap<String, String>,
}

impl FromStr for Passport {
    type Err = Error;
    fn from_str(s: &str) -> Result<Passport, Error> {
        let values = s
            .replace("\n", " ")
            .trim_end_matches(" ")
            .split(" ")
            .map(|kv| match kv.split(":").collect::<Vec<_>>()[..] {
                [key, value] => Ok((String::from(key), String::from(value))),
                _ => Err(Error::from(format!(
                    "unable to parse {:?} into key-value pair for {:?}",
                    kv, s
                ))),
            })
            .collect::<Result<_, _>>()?;
        Ok(Passport { values })
    }
}

fn contains_fields(fields: &[&str], ps: Vec<Passport>) -> Vec<Passport> {
    ps.into_iter()
        .filter(|p| {
            fields
                .iter()
                .all(|&f| p.values.contains_key(&String::from(f)))
        })
        .collect()
}

fn validate_fields(
    predicates: &HashMap<&str, fn(&str) -> bool>,
    ps: Vec<Passport>,
) -> Vec<Passport> {
    ps.into_iter()
        .filter(|Passport { values }| values.iter().all(|(k, v)| predicates[k as &str](&v)))
        .collect()
}

lazy_static! {
    //static ref PASSPORT_PREDICATES: HashMap<&'static str, Predicate<&'static str>> = [
    static ref PASSPORT_PREDICATES: HashMap<&'static str, RefPredicate<str>> = [
        //("byr", byr as fn(&str) -> bool),
        ("byr", byr as RefPredicate<str>),
        ("iyr", iyr),
        ("eyr", eyr),
        ("hgt", hgt),
        ("hcl", hcl),
        ("ecl", ecl),
        ("pid", pid),
        ("cid", cid),
    ]
    .into_iter()
    .cloned()
    .collect();
    static ref PASSPORT_FIELDS: Vec<&'static str> =
        PASSPORT_PREDICATES.iter().map(|(&k, _)| k).collect();
}

//type Predicate<T> = fn(T) -> bool; // nope
//type StrPredicate = fn(&str) -> bool; // yep
//type StrPredicate = for<'r> fn(&'r str) -> bool; // yep
type RefPredicate<T> = for<'r> fn(&'r T) -> bool;
type BoxedPredicate<T> = Box<dyn Fn(T) -> bool>;

/// (Birth Year) - four digits; at least 1920 and at most 2002.
fn byr(s: &str) -> bool {
    s.parse().map(is_clamped(1920, 2002)).unwrap_or(false)
}

/// (Issue Year) - four digits; at least 2010 and at most 2020.
fn iyr(s: &str) -> bool {
    s.parse().map(is_clamped(2010, 2020)).unwrap_or(false)
}

/// (Expiration Year) - four digits; at least 2020 and at most 2030.
fn eyr(s: &str) -> bool {
    s.parse().map(is_clamped(2020, 2030)).unwrap_or(false)
}

/// (Height) - a number followed by either cm or in:
///     If cm, the number must be at least 150 and at most 193.
///     If in, the number must be at least 59 and at most 76.
fn hgt(s: &str) -> bool {
    s.parse()
        .map(|height| match height {
            Centimeters(n) => is_clamped(150, 193)(n),
            Inches(n) => is_clamped(59, 76)(n),
        })
        .unwrap_or(false)
}

enum Height {
    Centimeters(usize),
    Inches(usize),
}
use Height::*;

impl FromStr for Height {
    type Err = Error;
    fn from_str(s: &str) -> Result<Height, Error> {
        HEIGHT
            .captures(s)
            .map(|cap| match &cap[2] {
                "cm" => Centimeters(cap[1].parse().unwrap()),
                "in" => Inches(cap[1].parse().unwrap()),
                _ => unsafe { std::hint::unreachable_unchecked() },
            })
            .ok_or(Error::from(format!("unable to parse height from {:?}", s)))
    }
}

/// (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
fn hcl(s: &str) -> bool {
    HAIR_COLOR.is_match(s)
}

/// (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
fn ecl(s: &str) -> bool {
    EYE_COLOR.is_match(s)
}

/// (Passport ID) - a nine-digit number, including leading zeroes.
fn pid(s: &str) -> bool {
    PASSPORT_ID.is_match(s)
}

lazy_static! {
    static ref HEIGHT: Regex = Regex::new(r"^([1-9]\d*)(cm|in)$").unwrap();
    static ref HAIR_COLOR: Regex = Regex::new(r"^#[0-9a-f]{6}").unwrap();
    static ref EYE_COLOR: Regex = Regex::new(r"^(?:amb|blu|brn|gry|grn|hzl|oth)$").unwrap();
    static ref PASSPORT_ID: Regex = Regex::new(r"^\d{9}$").unwrap();
}

/// (Country ID) - ignored, missing or not.
fn cid(_: &str) -> bool {
    true
}

fn is_clamped<T>(lo: T, hi: T) -> BoxedPredicate<T>
where
    T: PartialOrd + 'static,
{
    Box::new(move |t| lo <= t && t <= hi)
}
