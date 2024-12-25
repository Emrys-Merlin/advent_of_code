use std::cmp::Ordering;
use std::collections::HashSet;

fn parse_input(input: &str) -> (HashSet<(usize, usize)>, Vec<Vec<usize>>) {
    let mut ordering: HashSet<(usize, usize)> = HashSet::new();
    let mut books: Vec<Vec<usize>> = Vec::new();
    let mut first_part = true;
    for mut line in input.lines() {
        line = line.trim();
        if first_part {
            if line.is_empty() {
                first_part = false;
                continue;
            }
            let (parent, child) = line
                .split_once("|")
                .map(|(left, right)| {
                    (
                        left.parse::<usize>().unwrap(),
                        right.parse::<usize>().unwrap(),
                    )
                })
                .unwrap();
            ordering.insert((parent, child));
        } else {
            let book: Vec<usize> = line
                .split(",")
                .map(|x| x.parse::<usize>().unwrap())
                .collect();
            books.push(book);
        }
    }

    (ordering, books)
}

fn get_center(book: &Vec<usize>) -> Option<usize> {
    if book.len() % 2 == 0 {
        return None;
    }
    let mid = book.len() / 2;
    Some(book[mid])
}

fn task(input: &str, sorted_books: bool) -> String {
    let (ordering, mut books) = parse_input(input);

    let mut result = 0;

    for book in books.iter_mut() {
        let orig_book = book.clone();
        (*book).sort_by(|a, b| {
            if ordering.contains(&(*a, *b)) {
                return Ordering::Less;
            }
            if ordering.contains(&(*b, *a)) {
                return Ordering::Greater;
            }
            return Ordering::Equal;
        });

        if (*book == orig_book && sorted_books) || (!sorted_books && *book != orig_book) {
            result += get_center(book).expect("No center found");
        }
    }

    result.to_string()
}

pub fn task01(input: &str) -> String {
    return task(input, true);
}

pub fn task02(input: &str) -> String {
    return task(input, false);
}
