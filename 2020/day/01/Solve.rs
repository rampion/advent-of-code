use std::collections::HashSet;
use std::env;
use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn fail<T>(msg: String) -> Result<T, Box<dyn Error>> {
    Err(msg.into())
}

fn main() -> Result<(), Box<dyn Error>> {
    let file = match &env::args().collect::<Vec<String>>()[1..] {
        [path] => File::open(path).map_err(Box::from),
        args => fail(format!("Expected one argument, found {:?}", args)),
    }?;

    let input: Vec<u32> = BufReader::new(file)
        .lines()
        .map(|read| read?.parse().map_err(Box::from))
        .collect::<Result<_, Box<dyn Error>>>()?;

    let input_set: HashSet<u32> = input.iter().cloned().collect();
    let paired: HashSet<u32> = input_set
        .intersection(&input_set.iter().cloned().map(|n| 2020 - n).collect())
        .cloned()
        .collect();

    println!("{:?}", paired);
    if paired.len() > 0 {
        println!("{}", paired.into_iter().product::<u32>());
    } else {
        fail("no pair that adds to 2020 found".to_owned())?;
    }

    for (ix, a) in input.iter().enumerate() {
        for (jx, b) in input[ix + 1..].iter().enumerate() {
            for c in &input[jx + 1..] {
                if a + b + c == 2020 {
                    println!("{} * {} * {} = {}", a, b, c, a * b * c);
                }
            }
        }
    }

    Ok(())
}
