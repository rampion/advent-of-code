use std::error;

fn main() -> Result<(), Error> {
    for filename in std::env::args().skip(1) {
        println!("{}", filename);

        let instructions = parse_instructions(&std::fs::read_to_string(filename)?)?;

        println!("\tpart 1: manhattan distance from current location to end of instructions");
        println!("\t{}", part1(&instructions));

        println!(
            "\tpart 2: manhattan distance from current location to end of waypoint instructions"
        );
        println!("\t{}", part2(&instructions));
    }

    Ok(())
}

////////////////////////////////////////////////////////////////////////////////

fn parse_instructions(src: &str) -> Result<Vec<Instruction>, Error> {
    src.lines().map(parse_instruction).collect()
}

macro_rules! error {
    ($fmt:literal $(, $arg:expr)*) => {
        return Err(Error::from(format!($fmt $(, $arg)*)))
    }
}

fn parse_instruction(src: &str) -> Result<Instruction, Error> {
    let cnt = src[1..].parse()?;
    let deg = parse_degrees(cnt);

    Ok(match &src[0..1] {
        "N" => Move(cnt, 0),
        "E" => Move(0, cnt),
        "S" => Move(-cnt, 0),
        "W" => Move(0, -cnt),
        "L" => Turn(-deg?),
        "R" => Turn(deg?),
        "F" => Forward(cnt),
        action => error!(
            "Unexpected action {:?}, expected one of N/E/S/W/L/R/F",
            action
        ),
    })
}

fn parse_degrees(cnt: i32) -> Result<Degrees, Error> {
    Ok(match cnt {
        0 => Zero,
        90 => Ninety,
        180 => OneEighty,
        270 => TwoSeventy,
        _ => error!("Unexpected angle {}°, expected increments of 90°", cnt),
    })
}

////////////////////////////////////////////////////////////////////////////////

fn part1(instructions: &[Instruction]) -> i32 {
    let mut heading = Ninety;
    let mut latitude = 0;
    let mut longitude = 0;

    for &instruction in instructions {
        match instruction {
            Move(dlat, dlon) => {
                latitude += dlat;
                longitude += dlon;
            }
            Turn(deg) => heading += deg,
            Forward(delta) => match heading {
                Zero => latitude += delta,
                Ninety => longitude += delta,
                OneEighty => latitude -= delta,
                TwoSeventy => longitude -= delta,
            },
        }
    }

    latitude.abs() + longitude.abs()
}

////////////////////////////////////////////////////////////////////////////////

fn part2(instructions: &[Instruction]) -> i32 {
    let mut ship_lat = 0;
    let mut ship_lon = 0;
    let mut waypoint_lat = 1;
    let mut waypoint_lon = 10;

    for &instruction in instructions {
        match instruction {
            Move(dlat, dlon) => {
                waypoint_lat += dlat;
                waypoint_lon += dlon;
            }
            Turn(Zero) => {}
            Turn(Ninety) => {
                std::mem::swap(&mut waypoint_lat, &mut waypoint_lon);
                waypoint_lat *= -1;
            }
            Turn(OneEighty) => {
                waypoint_lat *= -1;
                waypoint_lon *= -1;
            }
            Turn(TwoSeventy) => {
                std::mem::swap(&mut waypoint_lat, &mut waypoint_lon);
                waypoint_lon *= -1;
            }
            Forward(delta) => {
                ship_lat += delta * waypoint_lat;
                ship_lon += delta * waypoint_lon;
            }
        }
    }

    ship_lat.abs() + ship_lon.abs()
}

////////////////////////////////////////////////////////////////////////////////

type Error = Box<dyn error::Error>;

#[derive(Clone, Copy, Debug)]
enum Instruction {
    Move(i32, i32),
    Turn(Degrees),
    Forward(i32),
}
use Instruction::*;

#[derive(Clone, Copy, Debug)]
enum Degrees {
    Zero,
    Ninety,
    OneEighty,
    TwoSeventy,
}
use Degrees::*;

impl std::ops::AddAssign for Degrees {
    fn add_assign(&mut self, other: Degrees) {
        *self = match (*self, other) {
            (Zero, deg) => deg,
            (deg, Zero) => deg,
            (Ninety, Ninety) => OneEighty,
            (Ninety, OneEighty) => TwoSeventy,
            (OneEighty, Ninety) => TwoSeventy,
            (Ninety, TwoSeventy) => Zero,
            (TwoSeventy, Ninety) => Zero,
            (OneEighty, OneEighty) => Zero,
            (OneEighty, TwoSeventy) => Ninety,
            (TwoSeventy, OneEighty) => Ninety,
            (TwoSeventy, TwoSeventy) => OneEighty,
        }
    }
}

impl std::ops::Neg for Degrees {
    type Output = Self;

    fn neg(self) -> Degrees {
        match self {
            Zero => Zero,
            Ninety => TwoSeventy,
            OneEighty => OneEighty,
            TwoSeventy => Ninety,
        }
    }
}
