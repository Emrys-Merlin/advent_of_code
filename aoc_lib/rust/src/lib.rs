use pyo3::prelude::*;

mod graph;
mod numbertheory;

#[pymodule]
fn rust(_m: &Bound<'_, PyModule>) -> PyResult<()> {
    let nt = PyModule::new(_m.py(), "numbertheory")?;
    _m.add_submodule(&nt)?;
    numbertheory::register(&nt)?;
    _m.py()
        .import("sys")?
        .getattr("modules")?
        .set_item("aoc_lib.rust.numbertheory", &nt)?;

    let g = PyModule::new(_m.py(), "graph")?;
    _m.add_submodule(&g)?;
    graph::register(&g)?;
    _m.py()
        .import("sys")?
        .getattr("modules")?
        .set_item("aoc_lib.rust.graph", &g)?;
    Ok(())
}
