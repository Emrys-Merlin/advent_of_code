#![allow(dead_code)]
pub mod core;
use self::core::extended_euclidean_algorithm as eea_inner;
use self::core::gcd as gcd_inner;
use self::core::lcm as lcm_inner;
use pyo3::prelude::*;

#[pyfunction]
pub fn gcd(m: i64, n: i64) -> u64 {
    gcd_inner(m.unsigned_abs(), n.unsigned_abs())
}

#[pyfunction]
pub fn extended_euclidean_algorithm(m: i64, n: i64) -> (i64, i64, i64) {
    eea_inner(m, n)
}

#[pyfunction]
pub fn lcm(m: i64, n: i64) -> i64 {
    lcm_inner(m, n)
}

pub fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(gcd, m)?)?;
    m.add_function(wrap_pyfunction!(extended_euclidean_algorithm, m)?)?;
    m.add_function(wrap_pyfunction!(lcm, m)?)?;
    Ok(())
}
