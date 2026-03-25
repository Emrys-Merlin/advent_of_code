use pyo3::prelude::*;

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
    Ok(())
}
