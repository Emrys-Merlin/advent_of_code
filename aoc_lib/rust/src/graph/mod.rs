mod kruskal;

use pyo3::prelude::*;

pub fn register(parent: &Bound<'_, PyModule>) -> PyResult<()> {
    let m = PyModule::new(parent.py(), "kruskal")?;
    kruskal::register(&m)?;
    parent.add_submodule(&m)?;

    parent
        .py()
        .import("sys")?
        .getattr("modules")?
        .set_item("aoc_lib.rust.graph.kruskal", &m)?;
    Ok(())
}
