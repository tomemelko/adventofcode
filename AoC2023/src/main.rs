use std::env;
use std::fs;

mod day1;
mod day2;

fn main() {
    let args: Vec<String> = env::args().collect();
    let run_all = &args.get(1).is_some_and(|x| x == "all");
    let hard_mode = &args.get(2).is_some_and(|x| x == "hard");
    let runners = [
      day1::day1,
      day2::day2,
    ];
    let run_day = |i: usize| {
        let content = read_file_for_day(i + 1, *hard_mode);
        runners[i](&content);
    };
    if *run_all {
        for i in 0..runners.len() {
            run_day(i);
        }
    } else {
        let index = runners.len() - 1;
        run_day(index);
    }
}

fn read_file_for_day(n: usize, hard_mode: bool) -> String {
    return fs::read_to_string(format!(
        "src/data/day{}_{}.txt",
        n,
        match hard_mode {
            false => "easy",
            true => "hard",
        }
    ))
    .expect("Should have been able to read the file");
}
