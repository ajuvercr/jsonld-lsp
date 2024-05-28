use std::ops::{Index, IndexMut};

pub trait ParentElement {
    type I: Iterator<Item = usize>;
    fn iter(&self) -> Self::I;
}

#[derive(Debug)]
pub struct ParentingSystem<E> {
    pub objects: Vec<E>,
    parents: Vec<usize>,
    pub start: Option<usize>,
}

impl<E> Default for ParentingSystem<E> {
    fn default() -> Self {
        Self {
            objects: Vec::new(),
            parents: Vec::new(),
            start: None,
        }
    }
}

impl<E> ParentingSystem<E> {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            parents: Vec::new(),
            start: None,
        }
    }

    pub fn find<'a>(
        &'a self,
        mut idx: usize,
        mut found: impl FnMut(&'a E) -> bool,
    ) -> Option<(usize, &'a E)> {
        while !found(&self.objects[idx]) {
            idx = self.parent(idx)?.0;
        }
        Some((idx, &self.objects[idx]))
    }

    pub fn start_element(&self) -> Option<&E> {
        if let Some(x) = self.start {
            Some(&self[x])
        } else {
            None
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
    pub fn iter_desc<'a>(
        &'a self,
        self_first: bool,
        start: Option<usize>,
    ) -> impl Iterator<Item = &'a E> {
        if let Some(idx) = start.or(self.start) {
            let root_iter = &self[idx];
            Pos::Some(DescendIter {
                first: self_first,
                self_first,
                parents: self,
                iters: vec![(root_iter.iter(), idx)],
            })
        } else {
            Pos::Nope
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

pub enum Pos<T> {
    Nope,
    Some(T),
}

pub struct DescendIter<'a, E: ParentElement> {
    first: bool,
    parents: &'a ParentingSystem<E>,
    iters: Vec<(E::I, usize)>,
    self_first: bool,
}

impl<'a, E: ParentElement> Iterator for Pos<DescendIter<'a, E>> {
    type Item = &'a E;

    fn next(&mut self) -> Option<Self::Item> {
        let this = match self {
            Pos::Nope => return None,
            Pos::Some(ref mut x) => x,
        };
        let (mut x, index) = this.iters.pop()?;

        if this.first && this.self_first {
            this.first = false;
            if this.self_first {
                this.iters.push((x, index));
                return Some(&this.parents[index]);
            }
        }

        if let Some(n) = x.next() {
            this.iters.push((x, index));
            this.iters.push((this.parents[n].iter(), n));
            if this.self_first {
                return Some(&this.parents[n]);
            }
        } else {
            if !this.self_first {
                this.iters.push((x, index));
                return Some(&this.parents[index]);
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
