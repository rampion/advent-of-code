use std::collections::HashSet;
use std::error;

type Error = Box<dyn error::Error>;

macro_rules! error {
    ($fmt:literal $(, $e:expr)*) => { Err(Error::from(format!($fmt $(, $e)*))) };
}

fn main() -> Result<(), Error> {
    for filename in std::env::args().skip(1) {
        println!("{}", filename);

        let bootcode = parse_bootcode(&std::fs::read_to_string(filename)?)?;

        println!("\tpart 1: value of accelerator when code loops for the first time");
        println!("\t{}", part1(&bootcode)?);

        println!("\tpart 2: value of accelerator when patched code exits cleanly");
        println!("\t{}", part2(bootcode)?);
    }

    Ok(())
}

fn part1(bootcode: &BootCode) -> Result<i32, Error> {
    match simulate(bootcode) {
        Ok(_) => error!("Expected code to loop, but exited cleanly"),
        Err(n) => Ok(n),
    }
}

fn part2(mut bootcode: BootCode) -> Result<i32, Error> {
    for ix in 0..bootcode.len() {
        let (orig, repl) = match bootcode[ix] {
            (Acc, _) => continue,
            (Jmp, _) => (Jmp, Nop),
            (Nop, _) => (Nop, Jmp),
        };
        bootcode[ix].0 = repl;
        match simulate(&bootcode) {
            Err(_) => bootcode[ix].0 = orig,
            Ok(n) => return Ok(n),
        }
    }

    error!("Expected a patch to exist, but code never exited cleanly")
}

fn simulate(bootcode: &BootCode) -> Result<i32, i32> {
    let mut visited = HashSet::new();
    let mut acc = 0;
    let mut ix = 0;

    while (0..bootcode.len() as i32).contains(&ix) {
        if visited.contains(&ix) {
            return Err(acc);
        }
        visited.insert(ix);

        match bootcode[ix as usize] {
            (Acc, arg) => {
                acc += arg;
                ix += 1
            }
            (Jmp, arg) => ix += arg,
            (Nop, _arg) => ix += 1,
        }
    }

    return Ok(acc);
}

type BootCode = Vec<Instruction>;
type Instruction = (Operation, i32);
enum Operation {
    Acc,
    Jmp,
    Nop,
}
use Operation::*;

fn parse_bootcode(src: &str) -> Result<BootCode, Error> {
    src.lines().map(parse_instruction).collect()
}

fn parse_instruction(src: &str) -> Result<Instruction, Error> {
    let (op, arg) = match src.split(" ").collect::<Vec<&str>>()[..] {
        [op, arg] => Ok((op, arg)),
        _ => error!("Malformed instruction: {:?}", src),
    }?;

    let op = match op {
        "acc" => Ok(Acc),
        "jmp" => Ok(Jmp),
        "nop" => Ok(Nop),
        op => error!("Unrecognized operation: {:?}", op),
    }?;

    let arg = arg.parse().expect("positive or negative integer");

    Ok((op, arg))
}
