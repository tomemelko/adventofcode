fn line_sum(line: &str, sm: &mut StateMachine) -> i32 {
    let mut first: i32 = 0;
    let mut last: i32 = 0;
    for (_, &item) in line.as_bytes().iter().enumerate() {
        let mut c = item;
        sm.update_state(c);
        c = match sm.result.clone() {
            Some(v) => v,
            None => c,
        };
        // println!("r {}", c);
        if c >= b'0' && c <= b'9' {
            let val = (c - b'0').try_into().unwrap();
            if first == 0 {
                first = val;
            }
            last = val;
        }
    }
    return (first * 10) + last;
}

struct MatcherState {
    idx: usize,
    chars: Vec<u8>,
    r: u8,
}

impl MatcherState {
    pub fn new(s: String, r: String) -> MatcherState {
        MatcherState {
            idx: 0,
            chars: s.as_bytes().to_owned(),
            r: r.as_bytes()[0].to_owned(),
        }
    }
}

struct StateMachine {
    matcher_states: Vec<MatcherState>,
    pub result: Option<u8>,
}

impl StateMachine {
    pub fn new() -> StateMachine {
        StateMachine {
            matcher_states: Vec::new(),
            result: None,
        }
    }

    pub fn add_string_matcher(&mut self, matcher: String, replacement: String) {
        self.matcher_states
            .push(MatcherState::new(matcher, replacement))
    }

    pub fn add_matchers(&mut self, pairs: &[[&str; 2]]) {
        for p in pairs {
            self.add_string_matcher(p[0].to_owned(), p[1].to_owned())
        }
    }

    pub fn update_state(&mut self, c: u8) {
        // println!("c: {}", c);
        self.result = None;
        for matcher in self.matcher_states.iter_mut() {
            if matcher.chars.get(matcher.idx).is_some_and(|x| *x == c) {
                matcher.idx += 1;
            } else {
                matcher.idx = match matcher.chars.get(0).is_some_and(|x| *x == c) {
                    true => 1,
                    false => 0,
                };
            }
            // println!("m: i: {} cs: {:?}", matcher.idx, matcher.chars);

            if matcher.chars.len() == matcher.idx {
                // println!("v: {}", matcher.r - b'0');
                self.result = Some(matcher.r);
            }
        }
    }

    pub fn reset_indicies(&mut self) {
        for matcher in self.matcher_states.iter_mut() {
            matcher.idx = 0;
        }
    }
}

pub fn day1(raw_data: &str) {
    let mut sum = 0;
    let mut sm = StateMachine::new();
    for str in raw_data.split('\n') {
        sum += line_sum(str, &mut sm);
    }
    println!("Part 1 result: {:?}", sum);

    sum = 0;
    sm = StateMachine::new();

    sm.add_matchers(&[
        ["one", "1"],
        ["two", "2"],
        ["three", "3"],
        ["four", "4"],
        ["five", "5"],
        ["six", "6"],
        ["seven", "7"],
        ["eight", "8"],
        ["nine", "9"],
    ]);

    for str in raw_data.split('\n') {
        sum += line_sum(str, &mut sm);
        sm.reset_indicies();
    }
    println!("Part 2 result: {:?}", sum);
}

#[cfg(test)]
mod tests {
    use crate::day1::StateMachine;

    #[test]
    fn it_works() {
        let mut sm = StateMachine::new();
        sm.add_matchers(&[
            ["one", "1"],
            ["two", "2"],
            ["three", "3"],
            ["four", "4"],
            ["five", "5"],
            ["six", "6"],
            ["seven", "7"],
            ["eight", "8"],
            ["nine", "9"],
        ]);
        assert_eq!(crate::day1::line_sum("1v", &mut sm), 11);
        assert_eq!(crate::day1::line_sum("one7eighteightsixqkfsm", &mut sm), 16);
        assert_eq!(
            crate::day1::line_sum("44jkrsmcthreekktxlnnzjdslhfsmzl", &mut sm),
            43
        );
        assert_eq!(crate::day1::line_sum("dmhxlbsixh35", &mut sm), 65);
        assert_eq!(crate::day1::line_sum("four8ninetwofour864", &mut sm), 44);
        assert_eq!(crate::day1::line_sum("5threezmcq", &mut sm), 53);
        assert_eq!(crate::day1::line_sum("6ninefive7", &mut sm), 67);
        assert_eq!(crate::day1::line_sum("4nine9twooneeightwoz", &mut sm), 42);
        assert_eq!(crate::day1::line_sum("5klvpcfxpkhdhx717", &mut sm), 57);
        assert_eq!(crate::day1::line_sum("6stgznine4vhnsnhts9", &mut sm), 69);
        assert_eq!(
            crate::day1::line_sum("9threeone98seven1vnnvgxslf", &mut sm),
            91
        );
        assert_eq!(
            crate::day1::line_sum("sixeighteightztpdhvt2zqjstmzmtzgsfthreezzhhdr", &mut sm),
            63
        );
        assert_eq!(
            crate::day1::line_sum("twodtbkqsjgtwohfnsqcrmpjfourhkpnsfdkfive6four", &mut sm),
            24
        );
        assert_eq!(crate::day1::line_sum("ggrxkrdzmthree3", &mut sm), 33);
        assert_eq!(
            crate::day1::line_sum("sixlflcmmjrs5fivenine488", &mut sm),
            68
        );
        assert_eq!(
            crate::day1::line_sum("four94hmhvlczssonedvgchseven6", &mut sm),
            46
        );
        assert_eq!(
            crate::day1::line_sum("ssoneightfbfctjqv43psixsevenslqsfpkb1", &mut sm),
            11
        );
        assert_eq!(
            crate::day1::line_sum("bdpnkb9eightnvtwojxbztssqfmninethree", &mut sm),
            93
        );
        assert_eq!(
            crate::day1::line_sum("cpcnkvdbrqrxtfnmzbqgffivesix91fivehgrv", &mut sm),
            55
        );
        assert_eq!(crate::day1::line_sum("five5495eight2", &mut sm), 52);
        assert_eq!(
            crate::day1::line_sum("7foursix93seventwonbhtmfrbqgq", &mut sm),
            72
        );
        assert_eq!(
            crate::day1::line_sum("tpqhxqqxpcnmlhqhkz123ninefive", &mut sm),
            15
        );
        assert_eq!(crate::day1::line_sum("knqxmrrmninegr4", &mut sm), 94);
        assert_eq!(crate::day1::line_sum("14qhlbkthreellvnqpfpbb", &mut sm), 13);
        assert_eq!(crate::day1::line_sum("7eightcrlb6eightthree7", &mut sm), 77);
        assert_eq!(crate::day1::line_sum("twom3", &mut sm), 23);
        assert_eq!(
            crate::day1::line_sum("gtzdljfdzpdg4zbnzbnxmpcpfsevennine3", &mut sm),
            43
        );
        assert_eq!(
            crate::day1::line_sum("svfjvnninefourpqsdmjcfhvccnjkpf8", &mut sm),
            98
        );
        assert_eq!(
            crate::day1::line_sum("dzmoneighttwovk5tvpnmxfive", &mut sm),
            15
        );
        assert_eq!(crate::day1::line_sum("88msthvt4vbmnbrzjone", &mut sm), 81);
        assert_eq!(crate::day1::line_sum("nbgcs8nine", &mut sm), 89);
        assert_eq!(
            crate::day1::line_sum("4three53pczsx1sevenmzmtrzz", &mut sm),
            47
        );
        assert_eq!(crate::day1::line_sum("four24qphdrxfsf", &mut sm), 44);
        assert_eq!(crate::day1::line_sum("gdgj3f", &mut sm), 33);
        assert_eq!(
            crate::day1::line_sum("hthphptmmtwo7sixsevenoneightls", &mut sm),
            28
        );
        assert_eq!(
            crate::day1::line_sum("qxbhjmmqsixfkfn36three6", &mut sm),
            66
        );
        assert_eq!(
            crate::day1::line_sum("eightmkmdtvkctkvptsbckzpnkhpskdmp3", &mut sm),
            83
        );
        assert_eq!(
            crate::day1::line_sum("six2twobgzsfsptlqnine42xtmdprjqc", &mut sm),
            62
        );
        assert_eq!(crate::day1::line_sum("pxreightwo7", &mut sm), 87);
        assert_eq!(crate::day1::line_sum("oneightwo", &mut sm), 12);
    }
}
