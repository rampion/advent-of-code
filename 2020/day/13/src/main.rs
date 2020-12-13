fn main() {
    for filename in std::env::args().skip(1) {
        println!("{}", filename);

        let scenario = parse_scenario(&std::fs::read_to_string(filename).unwrap());

        println!("\tpart 1: ID of earliest bus * number of minutes you'll need to wait");
        println!("\t{}", part1(&scenario));

        println!("\tpart 2: earliest timestamp that results in the buses departing one per minute in order");
        println!("\t{}", part2(&scenario));
    }
}

////////////////////////////////////////////////////////////////////////////////

fn parse_scenario(src: &str) -> Scenario {
    match &src.lines().collect::<Vec<&str>>()[..] {
        [now, buses] => (
            now.parse().unwrap(),
            buses.split(',').map(parse_bus).collect(),
        ),
        lines => panic!("unparsable scenario {:?}", lines),
    }
}

fn parse_bus(src: &str) -> Bus {
    match src {
        "x" => None,
        _ => Some(src.parse().unwrap()),
    }
}

////////////////////////////////////////////////////////////////////////////////

fn part1((now, buses): &Scenario) -> Time {
    let (wait, bus_id) = buses
        .iter()
        .flatten()
        .map(|interval| (interval - now % interval, interval))
        .min_by_key(|p| p.0)
        .unwrap();
    wait * bus_id
}
////////////////////////////////////////////////////////////////////////////////

fn part2((_, buses): &Scenario) -> Time {
    let lcm: i64 = buses.iter().flatten().product();

    buses
        .iter()
        .enumerate()
        .map(|(c, &bus)| match bus {
            None => 0,
            Some(p) => {
                let q = lcm / p;
                let qinv = inverse_modulo(q, p).expect("bus intervals are coprime");
                (p - (c as i64 * qinv) % p) * q
            }
        })
        .sum::<i64>()
        % lcm
}

mod euclidean_algorithm {
    use std::ops::*;

    macro_rules! flip {
        ($f:expr) => {
            |x, y| $f(y, x)
        };
    }

    pub fn gcd<T>(mut a: T, mut b: T) -> T
    where
        T: PartialOrd + Rem<Output = T> + Copy + From<i64>,
    {
        if a > b {
            std::mem::swap(&mut a, &mut b);
        }

        while a != T::from(0) {
            b = flip!(std::mem::replace)(b % a, &mut a);
        }

        b
    }

    pub fn inverse_modulo<T>(u: T, v: T) -> Option<T>
    where
        T: Copy
            + From<i64>
            + PartialOrd
            + PartialEq
            + Div<Output = T>
            + Sub<Output = T>
            + Mul<Output = T>
            + Neg<Output = T>,
    {
        let (a, b) = LinearCombination::initial(u, v);
        let r = gcd(a, b);

        if r.value == T::from(1) {
            Some(r.u_coef)
        } else {
            None
        }
    }

    #[derive(Copy, Clone)]
    struct LinearCombination<T> {
        value: T,
        u_coef: T,
        v_coef: T,
    }

    impl<T> Rem for LinearCombination<T>
    where
        T: Div<Output = T> + Sub<Output = T> + Mul<Output = T> + Copy,
    {
        type Output = Self;
        fn rem(self, rhs: Self) -> Self {
            self - rhs * (self.value / rhs.value)
        }
    }

    impl<T> Sub for LinearCombination<T>
    where
        T: Sub<Output = T>,
    {
        type Output = Self;
        fn sub(self, rhs: Self) -> Self {
            LinearCombination {
                value: self.value - rhs.value,
                u_coef: self.u_coef - rhs.u_coef,
                v_coef: self.v_coef - rhs.v_coef,
            }
        }
    }

    impl<T> Mul<T> for LinearCombination<T>
    where
        T: Mul<Output = T> + Copy,
    {
        type Output = Self;
        fn mul(self, rhs: T) -> Self {
            LinearCombination {
                value: self.value * rhs,
                u_coef: self.u_coef * rhs,
                v_coef: self.v_coef * rhs,
            }
        }
    }

    impl<T> PartialOrd for LinearCombination<T>
    where
        T: PartialOrd,
    {
        fn partial_cmp(&self, rhs: &Self) -> Option<std::cmp::Ordering> {
            self.value.partial_cmp(&rhs.value)
        }
    }

    impl<T> PartialEq for LinearCombination<T>
    where
        T: PartialEq,
    {
        fn eq(&self, rhs: &Self) -> bool {
            self.value.eq(&rhs.value)
        }
    }

    impl<T> From<i64> for LinearCombination<T>
    where
        T: From<i64>,
    {
        fn from(v: i64) -> Self {
            LinearCombination {
                value: T::from(v),
                u_coef: T::from(0),
                v_coef: T::from(0),
            }
        }
    }

    impl<T> LinearCombination<T>
    where
        T: PartialOrd + From<i64> + Neg<Output = T> + Copy,
    {
        fn initial(u: T, v: T) -> (Self, Self) {
            let zero = T::from(0);
            let one = T::from(1);

            let (value, u_coef) = if u >= zero { (u, one) } else { (-u, -one) };
            let a = LinearCombination {
                value,
                u_coef,
                v_coef: zero,
            };
            let (value, v_coef) = if v >= zero { (v, one) } else { (-v, -one) };
            let b = LinearCombination {
                value,
                u_coef: zero,
                v_coef,
            };
            (a, b)
        }
    }
}

use euclidean_algorithm::inverse_modulo;

////////////////////////////////////////////////////////////////////////////////

type Scenario = (Time, Vec<Bus>);
type Time = i64;
type Bus = Option<Time>;
