use std::error;

type Error = Box<dyn error::Error>;

macro_rules! error {
    ($fmt:literal $(, $e:expr)*) => { Error::from(format!($fmt $(, $e)*)) };
}

fn main() -> Result<(), Error> {
    for filename in std::env::args().skip(1) {
        println!("{}", filename);

        let window = parse_window(&filename)?;
        let numbers = parse_numbers(&filename)?;
        let invalid_number = part1(window, &numbers).ok_or(error!(
            "expected a value that isn't the sum of two of the previous {}",
            window
        ))?;

        println!("\tpart 1: first number that isn't the sum of two distinct values from the {} prior numbers", window);
        println!("\t{}", invalid_number);

        println!("\tpart 2: encryption weakness of list of numbers");
        println!(
            "\t{}",
            part2(invalid_number, &numbers).ok_or(error!(
                "expected a non-trivial subsequence to sum to the invalid number"
            ))?
        );
    }

    Ok(())
}

fn part2(target: u64, numbers: &[u64]) -> Option<u64> {
    let mut first = 0;
    let mut total = 0;

    for (last, &a) in numbers.iter().enumerate() {
        total += a;
        while total > target {
            total -= numbers[first];
            first += 1;
        }

        if total == target && first < last {
            let summands = &numbers[first..last + 1];
            let &min = summands
                .iter()
                .min()
                .expect("non empty range has a minimum value");
            let &max = summands
                .iter()
                .max()
                .expect("non empty range has a maximum value");
            return Some(min + max);
        }
    }

    None
}

fn part1(window: usize, numbers: &[u64]) -> Option<u64> {
    use std::collections::hash_map::Entry::*;
    use std::collections::HashMap;

    let mut sum_counts = HashMap::new();

    for (i, &a) in numbers[0..window].iter().enumerate() {
        for &b in &numbers[i + 1..window] {
            if a != b {
                *sum_counts.entry(a + b).or_insert(0) += 1;
            }
        }
    }

    for (i, &new) in numbers[window..].iter().enumerate() {
        if sum_counts.get(&new).map_or(true, |&count| count == 0) {
            return Some(new);
        }

        let old = numbers[i];
        for &num in &numbers[i + 1..i + window] {
            if num != old {
                let mut entry = match sum_counts.entry(old + num) {
                    Occupied(entry) => entry,
                    Vacant(_) => panic!("windowing error"),
                };

                *entry.get_mut() -= 1;
                if *entry.get() == 0 {
                    entry.remove();
                }
            }

            if num != new {
                *sum_counts.entry(num + new).or_insert(0) += 1;
            }
        }
    }

    None
}

fn parse_numbers(filename: &str) -> Result<Vec<u64>, Error> {
    std::fs::read_to_string(filename)?
        .lines()
        .map(|line| line.parse().map_err(Error::from))
        .collect()
}

fn parse_window(filename: &str) -> Result<usize, Error> {
    Ok(filename[filename
        .rfind(".")
        .ok_or(error!("expected filename to match <prefix>.<window-size>"))?
        + 1..]
        .parse()?)
}
