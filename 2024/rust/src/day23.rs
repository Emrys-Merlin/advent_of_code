use std::collections::{HashMap, HashSet};

fn parse_input(input: &str) -> HashMap<String, HashSet<String>> {
    let mut graph = HashMap::new();
    input.lines().for_each(|line| {
        let (p, q) = line.trim().split_once("-").unwrap();
        let entry = graph.entry(p.to_string()).or_insert(HashSet::new());
        entry.insert(q.to_string());
        let entry = graph.entry(q.to_string()).or_insert(HashSet::new());
        entry.insert(p.to_string());
    });
    graph
}

fn sort_by_degree(graph: &HashMap<String, HashSet<String>>) -> Vec<String> {
    let mut sorted = graph.keys().map(|k| k.to_string()).collect::<Vec<String>>();
    // sort descending by degree
    sorted.sort_by(|a, b| graph.get(b).unwrap().len().cmp(&graph.get(a).unwrap().len()));
    sorted
}

fn count_triangles_with_t(graph: &mut HashMap<String, HashSet<String>>) -> usize {
    let nodes = sort_by_degree(graph);
    for node in nodes.iter() {
        println!("{}: {:?}", node, graph.get(node).unwrap().len());
    }
    let mut count = 0;

    for node in nodes.iter() {
        // let neighbors = graph.entry(node.clone()).or_insert(HashSet::new());
        let neighbors = graph.get(node).unwrap();
        for (i, neighbor) in neighbors.iter().enumerate() {
            for other_neighbor in neighbors.iter().skip(i+1) {
                if graph.get(neighbor).unwrap().contains(other_neighbor) {
                    // println!("{}-{}-{}", node, neighbor, other_neighbor);
                    if node.starts_with("t") || neighbor.starts_with("t") || other_neighbor.starts_with("t") {
                        count += 1;
                    }
                }

            }
        }

        for neighbor in graph.remove(node).unwrap() {
            graph.entry(neighbor).or_default().remove(node);
        }
    }
    count
}

fn bron_kerbosch(clique: &mut HashSet<String>, candidates: &mut HashSet<String>, excluded: &mut HashSet<String>, graph: &HashMap<String, HashSet<String>>,  maximal_clique: &mut HashSet<String>) {
    if candidates.is_empty() && excluded.is_empty() {
        if clique.len() > maximal_clique.len() {
            *maximal_clique = clique.clone();
        }
        return;
    }

    for node in candidates.clone().iter() {
        clique.insert(node.clone());
        let neighbors = graph.get(node).unwrap();
        let mut new_candidates = candidates.intersection(&neighbors).cloned().collect::<HashSet<String>>();
        let mut new_excluded = excluded.intersection(&neighbors).cloned().collect::<HashSet<String>>();
        bron_kerbosch(clique, &mut new_candidates, &mut new_excluded, graph, maximal_clique);
        clique.remove(node);
        candidates.remove(node);
        excluded.insert(node.clone());
    }
}

fn find_largest_clique_bk(graph: &HashMap<String, HashSet<String>>) -> HashSet<String> {
    let mut clique = HashSet::new();
    let mut candidates = HashSet::new();
    let mut excluded = HashSet::new();
    let mut maximal_clique = HashSet::new();

    for node in graph.keys() {
        candidates.insert(node.clone());
    }

    bron_kerbosch(&mut clique, &mut candidates, &mut excluded, graph, &mut maximal_clique);
    maximal_clique
}

pub fn task01(input: &str) -> String {
    let mut graph = parse_input(input);
    count_triangles_with_t(&mut graph).to_string()
}

pub fn task02(input: &str) -> String {
    let graph = parse_input(input);
    let clique = find_largest_clique_bk(&graph);
    let mut sorted_clique = clique.iter().cloned().collect::<Vec<String>>();
    sorted_clique.sort();
    sorted_clique.join(",")
}
