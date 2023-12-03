use regex::Regex;
use std::collections::HashSet;
use std::fmt;

#[derive(PartialEq, Eq, Hash, Clone)]
struct Point {
    x: i32,
    y: i32,
}
impl fmt::Debug for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "x: {:?} y: {:?}", self.x, self.y)
    }
}

struct Found {
    s: String,
    occupies: HashSet<Point>,
}
impl fmt::Debug for Found {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "s: {:?} m: {:?}", self.s, self.occupies)
    }
}

fn find_re_points<'a>(haystack: &'a str, r: &str) -> Vec<Found> {
    let mut v: Vec<Found> = vec![];
    let re = Regex::new(r).unwrap();
    for (row, line) in haystack.split("\n").enumerate() {
        for c in re.captures_iter(line) {
            let cap = c.get(0).unwrap();
            let mut s = HashSet::new();
            for col in (cap.start())..(cap.end()) {
                s.insert(Point {
                    x: col as i32,
                    y: row as i32,
                });
            }
            v.push(Found {
                s: cap.as_str().to_owned(),
                occupies: s,
            });
        }
    }
    return v;
}

fn find_neighbor_points(ps: &HashSet<Point>) -> HashSet<Point> {
    let mut r = HashSet::new();
    for p in ps {
        let lookaround = [-1, 0, 1];
        for yo in lookaround {
            for xo in lookaround {
                if !(xo == 0 && yo == 0) {
                    r.insert(Point {
                        x: p.x + xo,
                        y: p.y + yo,
                    });
                }
            }
        }
    }
    let poop: HashSet<Point> = &r - ps;
    return poop;
}

pub fn day3(raw_data: &str) {
    let mut p1sum = 0;
    let mut p2sum: i32 = 0;
    let mut symbolPoints = HashSet::new();
    for fs in find_re_points(raw_data, r"[-!#@$%^&*()_+|~=`{}\[\]:;'<>?,\/]") {
        if fs.s == "#" {
            println!("{:?}", fs.occupies)
        }
        symbolPoints.extend(fs.occupies);
    }
    for f in find_re_points(raw_data, r"[0-9]+") {
        if f.s == "633" {
            println!("{:?}", f.occupies);
            println!("{:?}", find_neighbor_points(&f.occupies));
        }
        if find_neighbor_points(&f.occupies)
            .intersection(&symbolPoints)
            .any(|_| true)
        {
            println!("{}", f.s.parse::<i32>().unwrap());
            p1sum += f.s.parse::<i32>().unwrap()
        }
    }
    println!("Part 1 result: {p1sum:?}");
    println!("Part 2 result: {p2sum:?}");
}
