pub fn gcd(mut m: u64, mut n: u64) -> u64 {
    while n != 0 {
        (m, n) = (n, m % n)
    }
    m
}

pub fn extended_euclidean_algorithm(m: i64, n: i64) -> (i64, i64, i64) {
    let (mut old_r, mut r) = (m, n);
    let (mut old_s, mut s) = (1i64, 0i64);
    let (mut old_t, mut t) = (0i64, 1i64);
    let mut q: i64;

    while r != 0 {
        q = old_r / r;

        (old_r, r) = (r, old_r - q * r);
        (old_s, s) = (s, old_s - q * s);
        (old_t, t) = (t, old_t - q * t);
    }

    (old_r, old_s, old_t)
}

pub fn lcm(m: i64, n: i64) -> i64 {
    let g = gcd(m.unsigned_abs(), n.unsigned_abs()) as i64;

    m * (n / g)
}
