
#[derive(Debug)]
struct Interval {
    start: usize,
    end: usize,
    idx: Option<usize>,
}

impl Interval {
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn move_to(&mut self, idx: usize) {
        let delta = self.len();
        self.start = idx;
        self.end = idx + delta;
    }

    pub fn shorten(&mut self, delta: usize) -> bool{
        if delta > self.len() {
            return false;
        }
        self.start += delta;
        true
    }

    pub fn checksum(&self) -> Option<usize> {
        match self.idx {
            Some(idx) => Some((self.start..self.end).map(|i| i * idx).sum()),
            None => None,
        }
    }
}

fn parse_input01(input: &str) -> (Vec<(usize, usize)>, Vec<usize>) {
    let mut content_idx: usize = 0;
    let mut fs_idx: usize = 0;
    let mut empty: bool = false;
    let mut filesystem: Vec<(usize, usize)> = Vec::new();
    let mut empty_spaces: Vec<usize> = Vec::new();

    for character in input.lines().next().unwrap().trim().chars() {
        let size = character.to_string().parse::<usize>().unwrap();
        if empty {
            empty = false;
            for _ in 0..size {
                empty_spaces.push(fs_idx);
                fs_idx += 1;
            }
        } else {
            empty = true;
            for _ in 0..size {
                filesystem.push((fs_idx, content_idx));
                fs_idx += 1;
            }
            content_idx += 1;
        }

    }
    filesystem.reverse();
    (filesystem, empty_spaces)
}

fn parse_input02(input: &str) -> (Vec<Interval>, Vec<Interval>) {
    let mut blocks = Vec::new();
    let mut empty_blocks = Vec::new();
    let mut empty = false;
    let mut fs_idx: usize = 0;
    let mut content_idx: usize = 0;

    for character in input.lines().next().unwrap().trim().chars() {
        let size = character.to_string().parse::<usize>().unwrap();
        if empty {
            empty = false;
            empty_blocks.push(Interval { start: fs_idx, end: fs_idx + size, idx: None });
        } else {
            empty = true;
            blocks.push(Interval { start: fs_idx, end: fs_idx + size, idx: Some(content_idx) });
            content_idx += 1;
        }
        fs_idx += size
    }

    (blocks, empty_blocks)
}

pub fn task01(input: &str) -> String {
    let (mut filesystem, empty_spaces) = parse_input01(input);

    for (idx, &empty_space) in empty_spaces.iter().enumerate() {
        if filesystem[idx].0 <= empty_space {
            break;
        }
        filesystem[idx].0 = empty_space;
    }

    filesystem.sort_by(|a, b| a.0.cmp(&b.0));

    filesystem.iter().map(|(fs_idx, content_idx)| *fs_idx**content_idx).sum::<usize>().to_string()
}

pub fn task02(input: &str) -> String {
    let (mut blocks, mut empty_blocks) = parse_input02(input);
    for block in blocks.iter_mut().rev() {
        for empty_block in empty_blocks.iter_mut() {
            if empty_block.len() < block.len() {
                continue;
            }
            if empty_block.start > block.start {
                break;
            }
            block.move_to(empty_block.start);
            empty_block.shorten(block.len());
            break;
        }
    }

    blocks.sort_by(|a, b| a.start.cmp(&b.start));

    blocks.iter().map(|block| block.checksum().unwrap()).sum::<usize>().to_string()
}
