use std::collections::HashSet;
use std::env;
use std::error;
use std::fs;

type Error = Box<dyn error::Error>;

fn main() -> Result<(), Error> {
    for input_file in env::args().skip(1) {
        println!("{}", input_file);
        let input = groups(&fs::read_to_string(input_file)?);

        println!(
            "\tpart 1: sum over groups of number of questions that some member answered yes to"
        );
        println!("\t{}", part1(&input));

        println!(
            "\tpart 2: sum over groups of number of questions that all members answered yes to"
        );
        println!("\t{}", part2(&input));
    }

    Ok(())
}

type Group = Vec<Member>;
type Member = HashSet<Answer>;
type Answer = char;

fn groups(s: &str) -> Vec<Group> {
    s.split("\n\n")
        .map(|s| s.lines().map(|line| line.chars().collect()).collect())
        .collect()
}

fn part1(gps: &Vec<Group>) -> usize {
    gps.iter()
        .map(|gp| {
            gp.iter()
                .fold(HashSet::new(), |acc, mem| acc.union(mem).cloned().collect())
                .len()
        })
        .sum()
}

fn part2(gps: &Vec<Group>) -> usize {
    gps.iter()
        .map(|gp| {
            gp.iter()
                .fold(None, |acc, mem| match acc {
                    None => Some(mem.clone()),
                    Some(acc) => Some(acc.intersection(mem).cloned().collect()),
                })
                .unwrap_or_else(HashSet::new)
                .len()
        })
        .sum()
}
