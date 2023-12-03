use std::collections::HashMap;

fn process_line(s: &str) -> HashMap::<String, i32> {
  let mut m = HashMap::<String, i32>::new();
  for game in s.split_once(": ").unwrap().1.split("; ").map(|s| s.split(", ").map(|x| x.split_once(" ").unwrap())) {
    for pair in game {
      println!("{pair:?}");
      let v = pair.0.parse().unwrap();
      m.insert(pair.1.to_owned(), match m.get(pair.1) {
        Some(o)=> std::cmp::max(v, *o),
        None => v,
      });
    }
  }
  return m;
}

fn check_game_possible(m: &HashMap::<String, i32>, bag: HashMap::<String, i32>) -> bool {
  let mut res = true;
  for k in bag.keys() {
    res = res && bag.get(k).unwrap() >= m.get(k).unwrap();
  }
  return res;
}

fn part1(line: &str) -> bool {
  let mut m = HashMap::<String, i32>::new();
  m.insert("red".to_owned(), 12);
  m.insert("green".to_owned(), 13);
  m.insert("blue".to_owned(), 14);
  return check_game_possible(&process_line(line), m);
}

fn part2(line: &str) -> i32 {
  let m = process_line(line);
  return (*m.get("red").unwrap()) * (*m.get("green").unwrap())*(*m.get("blue").unwrap())
}

pub fn day2(raw_data: &str) {
  let mut p1sum = 0;
  let mut p2sum = 0;
  for (i, str) in raw_data.split('\n').enumerate() {
    if part1(str) {
      p1sum += i + 1;
    }
    p2sum+= part2(str);
  }
  println!("Part 1 result: {p1sum:?}");
  println!("Part 2 result: {p2sum:?}");
}