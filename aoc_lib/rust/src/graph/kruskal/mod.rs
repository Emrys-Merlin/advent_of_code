#![allow(dead_code)]
pub mod core;
use self::core::kruskal as kruskal_inner;
use self::core::Edge;
use self::core::KruskalError;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::{PyMapping, PyTuple};
use std::collections::HashMap;

impl From<KruskalError> for PyErr {
    fn from(e: KruskalError) -> Self {
        PyValueError::new_err(e.to_string())
    }
}

#[pyfunction]
pub fn kruskal(
    _py: Python<'_>,
    n: usize,
    edges: &Bound<'_, PyMapping>,
) -> PyResult<(i64, Vec<Edge>)> {
    let edges_int: HashMap<Edge, usize> = edges
        .items()?
        .iter()
        .map(|item| {
            let pair = item.cast::<PyTuple>()?;
            let edge_any = pair.get_item(0)?;
            let edge = edge_any.cast::<PyTuple>()?;
            let u: usize = edge.get_item(0)?.extract()?;
            let v: usize = edge.get_item(1)?.extract()?;
            let w: usize = pair.get_item(1)?.extract()?;

            Ok(((u, v), w))
        })
        .collect::<PyResult<_>>()?;

    let result = kruskal_inner(n, edges_int)?;

    Ok((result.total, result.mst))
}

pub fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(kruskal, m)?)?;
    Ok(())
}
