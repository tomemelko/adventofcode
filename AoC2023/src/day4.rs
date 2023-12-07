use regex::Regex;
use std::collections::{HashSet, HashMap};

#[derive(Debug)]
struct Card {
    n: i32,
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
        n: Regex::new(r"\d+")
            .unwrap()
            .captures(c)
            .unwrap()
            .get(0)
            .unwrap()
            .as_str()
            .parse::<i32>()
            .unwrap(),
        winners: winners.to_owned(),
        have: have.to_owned(),
    };
}

fn get_sums(raw_data: &str) -> (i32, i32) {
    let mut part1 = 0;
    let mut part2 = 0;
    let mut counts = HashMap::new();
    for row in raw_data.split("\n") {
            let c = parse_line(row);
            let my_winners = c.winners.intersection(&c.have).count() as i32;
            let score = match my_winners.cmp(&0) {
                std::cmp::Ordering::Greater => 2_i32.pow((my_winners - 1) as u32),
                _ => 0,
            };
            // println!("{:?} ({}): {:?}", my_winners, score, c);
            part1 += score;
            let card_count: i32 = counts.get(&c.n).unwrap_or(&1).to_owned();
            part2 += card_count;
            for next_g in (c.n+1)..(c.n+my_winners+1) {
              counts.insert(next_g, counts.get(&next_g).unwrap_or(&1) + card_count);
            }
        };
    return (part1, part2);
}

pub fn day4(raw_data: &str) {
    let (p1sum, p2sum) = get_sums(raw_data);
    println!("Part 1 result: {p1sum:?}");
    println!("Part 2 result: {p2sum:?}");
}
