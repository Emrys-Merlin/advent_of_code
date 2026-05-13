use std::collections::HashMap;
use thiserror::Error;

pub type Edge = (usize, usize);

pub struct KruskalResult {
    pub total: i64,
    pub mst: Vec<Edge>,
}

#[derive(Debug, Error)]
pub enum KruskalError {
    #[error("graph is disconnected: {node_count} nodes but only {edge_count} edges in MST")]
    Disconnected {
        node_count: usize,
        edge_count: usize,
    },
}

pub fn kruskal(n: usize, edges: HashMap<Edge, usize>) -> Result<KruskalResult, KruskalError> {
    let mut weighted_edges: Vec<(usize, Edge)> = edges
        .iter()
        .map(|((u, v), weight)| (*weight, (*u, *v)))
        .collect();

    weighted_edges.sort_by_key(|(w, _)| *w);

    let mut mst = vec![];
    let mut du_set = DUSet::new(n);
    let mut total: usize = 0;

    weighted_edges.iter().for_each(|(weight, (u, v))| {
        if du_set.merge(*u, *v) {
            total += *weight;
            mst.push((*u, *v));
        }
    });

    if !du_set.connected() {
        return Err(KruskalError::Disconnected {
            node_count: n,
            edge_count: mst.len(),
        });
    }

    Ok(KruskalResult {
        total: total as i64,
        mst: mst,
    })
}

struct DUSet {
    parent: Vec<usize>,
    size: Vec<usize>,
}

impl DUSet {
    fn new(n: usize) -> Self {
        Self {
            parent: (0..n).collect(),
            size: vec![1; n],
        }
    }

    fn find(&mut self, mut x: usize) -> usize {
        while self.parent[x] != x {
            (x, self.parent[x]) = (self.parent[x], self.parent[self.parent[x]]);
        }
        x
    }

    fn merge(&mut self, x: usize, y: usize) -> bool {
        let rx = self.find(x);
        let ry = self.find(y);

        if rx == ry {
            return false;
        }

        let (small, large) = if self.size[rx] < self.size[ry] {
            (rx, ry)
        } else {
            (ry, rx)
        };
        self.parent[small] = large;
        self.size[large] += self.size[small];
        true
    }

    fn connected(&self) -> bool {
        self.size.iter().copied().max().unwrap_or(0) == self.size.len()
    }
}
