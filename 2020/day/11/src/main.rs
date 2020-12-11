use std::error;

type Error = Box<dyn error::Error>;

fn main() -> Result<(), Error> {
    for filename in std::env::args().skip(1) {
        println!("{}", filename);

        let grid = parse_grid(&std::fs::read_to_string(filename)?)?;

        println!("\tpart 1: number of filled seats in stable seating configuration");
        println!("\t{}", part1(grid.clone()));

        println!("\tpart 2: number of filled seats in revised stable seating configuration");
        println!("\t{}", part2(grid));
    }

    Ok(())
}

type Grid = Vec<Vec<Tile>>;
type Point = (usize, usize);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
enum Tile {
    Floor,
    Unstable,
    Full,
    Empty,
}
use Tile::*;

fn count_full(grid: Grid) -> usize {
    let mut num_full = 0;
    for row in grid {
        for tile in row {
            if Full == tile {
                num_full += 1;
            }
        }
    }
    num_full
}

fn stabilize(
    mut grid: Grid,
    neighbors: fn(&Grid, Point) -> Vec<(Point, Tile)>,
    threshold: usize,
) -> Grid {
    use std::collections::HashSet;

    let mut queue = HashSet::new();
    for (y, row) in grid.iter().enumerate() {
        for (x, &tile) in row.iter().enumerate() {
            if tile == Unstable {
                queue.insert((x, y));
            }
        }
    }

    while !queue.is_empty() {
        let mut fillable = vec![];
        let mut flagged = HashSet::new();

        for (x, y) in queue.into_iter() {
            let mut num_full = 0;
            let mut unstable = vec![];

            for (p, tile) in neighbors(&grid, (x, y)) {
                match tile {
                    Full => num_full += 1,
                    Unstable => unstable.push(p),
                    _ => (),
                }
            }

            if num_full + unstable.len() < threshold {
                fillable.push((x, y));
                flagged.extend(&unstable);
            }
        }

        for (x, y) in fillable.into_iter() {
            grid[y][x] = Full;
        }

        let mut unfillable = vec![];
        for (x, y) in flagged.into_iter() {
            if grid[y][x] == Full {
                continue;
            }
            grid[y][x] = Empty;
            unfillable.push((x, y));
        }

        queue = HashSet::new();
        for (x, y) in unfillable.into_iter() {
            for (p, tile) in neighbors(&grid, (x, y)) {
                if tile == Unstable {
                    queue.insert(p);
                }
            }
        }
    }

    grid
}

#[allow(unused)]
fn slow_stabilize(
    mut grid: Grid,
    neighbors: fn(&Grid, Point) -> Vec<(Point, Tile)>,
    threshold: usize,
) -> Grid {
    let height = grid.len();
    let width = grid[0].len();
    let mut unstable = true;

    while unstable {
        let mut updates = vec![];

        for y in 0..height {
            for x in 0..width {
                let tile = grid[y][x];

                if tile == Floor {
                    continue;
                }

                let mut num_full = 0;
                for (_p, tile) in neighbors(&grid, (x, y)) {
                    if tile == Full {
                        num_full += 1;
                    }
                }

                if num_full >= threshold && tile != Empty {
                    updates.push(((x, y), Empty));
                } else if num_full == 0 && tile != Full {
                    updates.push(((x, y), Full));
                }
            }
        }

        unstable = updates.len() > 0;

        for ((x, y), tile) in updates.into_iter() {
            grid[y][x] = tile;
        }
    }

    grid
}

const DELTAS: [(i64, i64); 8] = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];

fn adjacent(grid: &Grid, (x, y): Point) -> Vec<(Point, Tile)> {
    let height = grid.len() as i64;
    let width = grid[0].len() as i64;

    DELTAS
        .iter()
        .filter_map(|(dx, dy)| {
            let x = x as i64 + dx;
            let y = y as i64 + dy;
            if (0..height).contains(&y) && (0..width).contains(&x) {
                let x = x as usize;
                let y = y as usize;
                Some(((x, y), grid[y][x]))
            } else {
                None
            }
        })
        .collect()
}

fn visible(grid: &Grid, (x, y): Point) -> Vec<(Point, Tile)> {
    let height = grid.len() as i64;
    let width = grid[0].len() as i64;

    DELTAS
        .iter()
        .filter_map(|(dx, dy)| {
            let mut x = x as i64 + dx;
            let mut y = y as i64 + dy;

            while (0..height).contains(&y)
                && (0..width).contains(&x)
                && grid[y as usize][x as usize] == Floor
            {
                x += dx;
                y += dy;
            }

            if (0..height).contains(&y) && (0..width).contains(&x) {
                let x = x as usize;
                let y = y as usize;
                Some(((x, y), grid[y][x]))
            } else {
                None
            }
        })
        .collect()
}

fn part1(grid: Grid) -> usize {
    count_full(stabilize(grid, adjacent, 4))
}

fn part2(grid: Grid) -> usize {
    count_full(stabilize(grid, visible, 5))
}

fn parse_grid(src: &str) -> Result<Grid, Error> {
    src.lines().map(parse_tiles).collect()
}

fn parse_tiles(src: &str) -> Result<Vec<Tile>, Error> {
    src.chars().map(parse_tile).collect()
}

macro_rules! error {
    ($fmt:literal $(, $arg:expr)*) => { Error::from(format!($fmt $(, $arg)*)) }
}

fn parse_tile(src: char) -> Result<Tile, Error> {
    match src {
        'L' => Ok(Unstable),
        '.' => Ok(Floor),
        _ => Err(error!(
            "unrecognized grid character {:?}, expected 'L' or '.'",
            src
        )),
    }
}
