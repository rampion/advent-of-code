use std::error;

type Error = Box<dyn error::Error>;

/*
macro_rules! error {
    ($fmt:literal $(, $arg:expr)*) => { Error::from(format!($fmt $(, $arg)* )) };
}
*/

fn main() -> Result<(), Error> {
    for filename in std::env::args().skip(1) {
        print!("{}", filename);

        let mut adaptors: Vec<Joltage> = std::fs::read_to_string(filename)?
            .lines()
            .map(|line| line.parse().map_err(Error::from))
            .collect::<Result<_, Error>>()?;
        adaptors.sort();

        println!("\tpart 1: product of 1-joltage differences and 3-joltage differences");
        println!("\t{}", part1(&adaptors));

        println!("\tpart 2: number of different adaptor chains");
        println!("\t{}", part2(&adaptors));
    }
    Ok(())
}

type Joltage = usize;

fn part1(adaptors: &[Joltage]) -> u64 {
    let mut counts = [0; 4];
    let mut prev = 0;

    for &r in adaptors {
        counts[r - prev] += 1;
        prev = r;
    }
    counts[3] += 1;

    return counts[1] * counts[3];
}

fn part2(adaptors: &[Joltage]) -> u64 {
    let mut a = 0; // ways to add to n - 2
    let mut b = 0; // ways to add to n - 1
    let mut c = 1; // ways to add to n
    let mut n = 0;

    for &r in adaptors {
        match r - n {
            1 => {
                let d = a + b + c;
                a = b;
                b = c;
                c = d;
            }
            2 => {
                let d = 0;
                let e = b + c + d;
                a = c;
                b = d;
                c = e;
            }
            3 => {
                let d = 0;
                let e = 0;
                let f = c;
                a = d;
                b = e;
                c = f;
            }
            _ => {
                a = 0;
                b = 0;
                c = 0;
            }
        }
        n = r;
    }

    c
}
