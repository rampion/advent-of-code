use std::collections::HashMap;
use std::error;

type Error = Box<dyn error::Error>;

macro_rules! error {
    ($fmt:literal $(, $e:expr)*) => { Err(Error::from(format!($fmt $(, $e)*))) };
}

fn main() -> Result<(), Error> {
    for filename in std::env::args().skip(1) {
        println!("{}", filename);

        let rules = flatten(parse_rules(&std::fs::read_to_string(filename)?)?);

        println!("\tpart 1: how many different bags could recursively contain a shiny gold bag");
        println!("\t{}", part1(&rules));

        println!("\tpart 2: how many different bags are recursively contained in a shiny gold bag");
        println!("\t{}", part2(&rules));
    }

    Ok(())
}

fn parse_rules(src: &str) -> Result<Rules, Error> {
    src.lines().map(parse_rule).collect()
}

fn parse_rule(src: &str) -> Result<(Bag, Counts), Error> {
    let words: Vec<&str> = src.split_whitespace().collect();

    let bag = match &words[0..4] {
        [adverb, color, "bags", "contain"] => Ok(to_bag(adverb, color)),
        terms => error!(
            "expected \"<adverb> <color> bags contain\" but found {:?}",
            terms.join(" ")
        ),
    }?;

    let counts = match words[4..] {
        ["no", "other", "bags."] => Ok(HashMap::new()),
        _ => (4..words.len())
            .step_by(4)
            .map(|ix| match &words[ix..ix + 4] {
                [count, adverb, color, _bag] => {
                    Ok((to_bag(adverb, color), count.parse().map_err(Error::from)?))
                }
                terms => error!(
                    "expected \"<count> <adverb> <color> bags?[.,]\", but found {:?}",
                    terms.join(" ")
                ),
            })
            .collect::<Result<Counts, Error>>(),
    }?;

    Ok((bag, counts))
}

fn to_bag(adverb: &str, color: &str) -> Bag {
    (adverb.into(), color.into())
}

fn part1(rules: &Rules) -> usize {
    let shiny_gold_bag = to_bag("shiny", "gold");

    rules
        .values()
        .filter(|counts| counts.get(&shiny_gold_bag).cloned().unwrap_or(0) > 0)
        .length()
}

fn part2(rules: &Rules) -> usize {
    let shiny_gold_bag = to_bag("shiny", "gold");

    rules
        .get(&shiny_gold_bag)
        .map_or(0, |counts| counts.values().sum())
}

trait Length {
    fn length(self) -> usize;
}

impl<T> Length for T
where
    T: IntoIterator,
{
    fn length(self) -> usize {
        let mut length = 0;
        for _ in self {
            length += 1;
        }
        length
    }
}

type Rules = HashMap<Bag, Counts>;
type Counts = HashMap<Bag, usize>;
type Bag = (String, String);

fn scale_and_add_to(dst: &mut Counts, src: &Counts, multiplier: usize) {
    for (bag, &count) in src {
        *dst.entry(bag.clone()).or_insert(0) += multiplier * count;
    }
}

fn flatten(rules: Rules) -> Rules {
    //! calculate the nested counts using DFS

    let mut results = HashMap::new();

    for (outer_bag, counts) in rules.iter() {
        if results.contains_key(outer_bag) {
            continue;
        }

        let mut flattened = HashMap::new();
        let mut queue = counts.iter();
        let mut stack = Vec::new();

        loop {
            match queue.next() {
                Some((inner_bag, &inner_count)) => {
                    *flattened.entry(inner_bag.clone()).or_insert(0) += inner_count;

                    match results.get(inner_bag) {
                        Some(inner_flattened) => {
                            scale_and_add_to(&mut flattened, &inner_flattened, inner_count);
                        }
                        None => {
                            stack.push((flattened, queue, inner_bag, inner_count));
                            flattened = HashMap::new();
                            queue = rules[&inner_bag].iter();
                        }
                    }
                }
                None => match stack.pop() {
                    Some((mut outer_flattened, outer_queue, bag, count)) => {
                        scale_and_add_to(&mut outer_flattened, &flattened, count);
                        results.insert(bag.clone(), flattened);
                        flattened = outer_flattened;
                        queue = outer_queue;
                    }
                    None => break,
                },
            }
        }

        results.insert(outer_bag.clone(), flattened);
    }

    results
}
