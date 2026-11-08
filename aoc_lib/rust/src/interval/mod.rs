use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::sync::PyOnceLock;
use pyo3::types::PyType;

static ABSTRACT_INTERVAL: PyOnceLock<Py<PyType>> = PyOnceLock::new();

#[pyclass(frozen, eq, hash, ord)]
#[derive(PartialEq, Eq, Hash, PartialOrd)]
struct Interval {
    #[pyo3(get)]
    lo: isize,
    #[pyo3(get)]
    hi: isize,
}

#[pymethods]
impl Interval {
    #[new]
    fn new(lo: isize, hi: isize) -> PyResult<Self> {
        if hi < lo {
            return Err(PyValueError::new_err(format!(
                "lo must be <= hi, got lo={lo} and hi={hi}"
            )));
        }
        Ok(Self { lo, hi })
    }

    #[classmethod]
    fn from_closed(_cls: &Bound<'_, PyType>, lo: isize, hi: isize) -> PyResult<Self> {
        Self::new(lo, hi + 1)
    }

    fn __len__(&self) -> usize {
        self.hi.abs_diff(self.lo)
    }

    fn __contains__(&self, x: &Bound<'_, PyAny>) -> PyResult<bool> {
        if let Ok(other) = x.cast::<Interval>() {
            let other = other.get();
            return Ok(self.lo <= other.lo && other.hi <= self.hi);
        }
        if let Ok(n) = x.extract::<isize>() {
            return Ok(self.lo <= n && n < self.hi);
        }

        if is_abstract_interval(x)? {
            return Ok(match x.extract::<IntervalLike>() {
                Ok(o) => self.lo <= o.lo && o.hi <= self.hi,
                Err(_) => false,
            });
        }
        Ok(false)
    }
}

#[derive(FromPyObject)]
struct IntervalLike {
    lo: isize,
    hi: isize,
}

fn is_abstract_interval(x: &Bound<'_, PyAny>) -> PyResult<bool> {
    let proto =
        ABSTRACT_INTERVAL.import(x.py(), "aoc_lib.interface.interval", "AbstractInterval")?;
    x.is_instance(proto)
}

pub fn register(m: &Bound<'_, PyModule>) -> PyResult<()> {
    // m.add_function(wrap_pyfunction!(gcd, m)?)?;
    m.add_class::<Interval>()?;
    Ok(())
}
