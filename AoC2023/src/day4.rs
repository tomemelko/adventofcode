use regex::Regex;
use std::collections::HashSet;

#[derive(Debug)]
struct Card {
    _n: i32,
    winners: HashSet<i32>,
    have: HashSet<i32>,
}

fn parse_line(line: &str) -> Card {
    let mut winners = HashSet::new();
    let mut have = HashSet::new();
    let (c, game) = line.split_once(": ").unwrap();
    let (win_group, have_group) = game.split_once(" | ").unwrap();
    for group in Regex::new(r"\d+").unwrap().captures_iter(win_group) {
        winners.insert(group.get(0).unwrap().as_str().parse::<i32>().unwrap());
    }
    for group in Regex::new(r"\d+").unwrap().captures_iter(have_group) {
        have.insert(group.get(0).unwrap().as_str().parse::<i32>().unwrap());
    }
    return Card {
        _n: Regex::new(r"\d+").unwrap().captures(c).unwrap().get(0).unwrap().as_str().parse::<i32>().unwrap(),
        winners: winners.to_owned(),
        have: have.to_owned(),
    };
}

fn part1(raw_data: &str) -> i32 {
    return raw_data
        .split("\n")
        .map(|x| {
            let c = parse_line(x);
            let my_winners = c.winners.intersection(&c.have).count() as i32;
            let score = match my_winners.cmp(&0) {
              std::cmp::Ordering::Greater => 2_i32.pow((my_winners - 1) as u32),
              _ => 0
            };
            // println!("{:?} ({}): {:?}", my_winners, score, c);
            return score
        })
        .sum();
}

pub fn day4(raw_data: &str) {
    let p1sum = part1(raw_data);
    let p2sum: i32 = 0;
    println!("Part 1 result: {p1sum:?}");
    println!("Part 2 result: {p2sum:?}");
}
