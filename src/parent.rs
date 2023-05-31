use std::ops::{Index, IndexMut};

pub trait ParentElement {
    type I: Iterator<Item = usize>;
    fn iter(&self) -> Self::I;
}

#[derive(Default)]
pub struct ParentingSystem<E> {
    objects: Vec<E>,
    parents: Vec<usize>,
}

impl<E> ParentingSystem<E> {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            parents: Vec::new(),
        }
    }
    pub fn add(&mut self, item: E, parent: usize) -> usize {
        let out = self.parents.len();

        self.objects.push(item);
        self.parents.push(parent);

        return out;
    }

    pub fn parent(&self, idx: usize) -> Option<(usize, &E)> {
        if idx == 0 {
            None
        } else {
            let ids = self.parents[idx];
            Some((ids, &self.objects[ids]))
        }
    }

    pub fn set_parent(&mut self, item: usize, parent: usize) {
        self.parents[item] = parent;
    }

    pub fn parent_iter<'a>(&'a self, idx: usize) -> ParentIter<'a, E> {
        ParentIter { parents: self, idx }
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (usize, &'a E)> {
        self.objects.iter().enumerate()
    }
}

impl<E: ParentElement> ParentingSystem<E> {
    pub fn iter_desc<'a>(&'a self, self_first: bool) -> impl Iterator<Item = &'a E> {
        let root_iter = self[0].iter();
        DescendIter {
            first: self_first,
            self_first,
            parents: self,
            iters: vec![(root_iter, 0)],
        }
    }
}

impl<E> Index<usize> for ParentingSystem<E> {
    type Output = E;

    fn index(&self, index: usize) -> &Self::Output {
        &self.objects[index]
    }
}

impl<E> IndexMut<usize> for ParentingSystem<E> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.objects[index]
    }
}

pub struct DescendIter<'a, E: ParentElement> {
    first: bool,
    parents: &'a ParentingSystem<E>,
    iters: Vec<(E::I, usize)>,
    self_first: bool,
}

impl<'a, E: ParentElement> Iterator for DescendIter<'a, E> {
    type Item = &'a E;

    fn next(&mut self) -> Option<Self::Item> {
        let (mut x, index) = self.iters.pop()?;

        if self.first && self.self_first {
            self.first = false;
            if self.self_first {
                self.iters.push((x, index));
                return Some(&self.parents[index]);
            }
        }

        if let Some(n) = x.next() {
            self.iters.push((x, index));
            self.iters.push((self.parents[n].iter(), n));
            if self.self_first {
                return Some(&self.parents[n]);
            }
        } else {
            if !self.self_first {
                self.iters.push((x, index));
                return Some(&self.parents[index]);
            }
        }

        return self.next();
    }
}

pub struct ParentIter<'a, E> {
    parents: &'a ParentingSystem<E>,
    idx: usize,
}

impl<'a, E> Iterator for ParentIter<'a, E> {
    type Item = (usize, &'a E);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((idx, o)) = self.parents.parent(self.idx) {
            self.idx = idx;
            Some((idx, o))
        } else {
            None
        }
    }
}
