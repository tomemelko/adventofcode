use regex::Regex;
use std::collections::HashMap;
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
    start: Point,
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
                start: Point { x: cap.start() as i32, y: row as i32 },
                occupies: s,
            });
        }
    }
    return v;
}

fn find_neighbor_points_single(p: &Point) -> HashSet<Point> {
    let mut r = HashSet::new();
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
    return r;
}

fn find_neighbor_points(ps: &HashSet<Point>) -> HashSet<Point> {
    let mut r = HashSet::new();
    for p in ps {
        r.extend(find_neighbor_points_single(p))
    }
    let s: HashSet<Point> = &r - ps;
    return s;
}

fn get_sums(raw_data: &str) -> (i32, i32){
    let mut p1sum = 0;
    let mut p2sum: i32 = 0;

    let mut symbol_points = HashMap::new();
    for fs in find_re_points(raw_data, r"[-!#@$%^&*()_+|~=`{}\[\]:;'<>?,\/]") {
        let ps: Vec<Point> = fs.occupies.clone().drain().collect();
        symbol_points.insert(ps.get(0).unwrap().to_owned(), fs.s);
    }

    let mut symbol_neighbors: HashMap<Point, HashSet<(Point, i32)>> = HashMap::new();
    for f in find_re_points(raw_data, r"[0-9]+") {
        for np in find_neighbor_points(&f.occupies) {
            if symbol_points.contains_key(&np) {
				let parsed = f.s.parse::<i32>().unwrap();
                p1sum += parsed;

                if symbol_points.get(&np).unwrap() == "*" {
					let h = HashSet::new();
                    let mut vals = symbol_neighbors.get(&np).unwrap_or_else(|| &h).to_owned();
                    vals.insert((f.start.to_owned(), parsed));
                    symbol_neighbors.insert(np.to_owned(), vals.to_owned());
                    // println!("{:?}, {:?} ;;; {:?}", np, vals, symbol_neighbors);
                }
            }
        }
    }
    for (p, _s) in symbol_points {
        let res = symbol_neighbors.get(&p);
        if res.is_some_and(|x| x.len() == 2) {
            let f: Vec<(Point, i32)> = res.unwrap().into_iter().map(|b| b.to_owned()).collect();
			let ratio = f.get(0).unwrap().1 * f.get(1).unwrap().1;
            p2sum += ratio;
        }
    }

    return (p1sum, p2sum)
}

pub fn day3(raw_data: &str) {
    let sums = get_sums(raw_data);
    println!("Part 1 result: {:?}", sums.0);
    println!("Part 2 result: {:?}", sums.1);
}


#[cfg(test)]
mod tests {

    #[test]
    fn it_works() {
        let test_vectors = [
            (12, "4*3"),
            (12, "3*4"),
            (12, "3..\n*..\n4.."),
            (12, "3..\n.*.\n.4."),
            (12, "3..\n.*.\n4.."),
            (0, "3..\n..*\n4.."),
            (36, "3*8\n*..\n4.."),
            (425*425, ".425.\n..*..\n=.425"),
        ];
        for v in test_vectors {
            assert_eq!(crate::day3::get_sums(v.1).1, v.0);
        }
    }
}
