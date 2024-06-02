use std::sync::Arc;

use hashbrown::HashSet;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Class {
    Named(Arc<String>),
    Unnamed(Arc<String>, &'static str),
}

impl Class {
    pub fn as_str(&self) -> &str {
        match self {
            Class::Named(x) => x.as_str(),
            Class::Unnamed(x, _) => x.as_str(),
        }
    }
}

pub enum ClassRef<'a> {
    Named(&'a str),
    Unnamed(&'a str, &'static str),
}

impl<'a> ClassRef<'a> {
    fn eq(&self, other: &Class) -> bool {
        match (self, other) {
            (ClassRef::Named(a), Class::Named(b)) => *a == b.as_str(),
            (ClassRef::Unnamed(a1, a2), Class::Unnamed(b1, b2)) => *a1 == b1.as_str() && a2 == b2,
            _ => false,
        }
    }
}

pub trait ClassProvider {
    fn unnamed(&mut self, id: Option<&str>, ty: &'static str) -> usize;
    fn named(&mut self, id: &str) -> usize;

    fn add_subclass(&mut self, class: usize, subclass: usize);

    fn class(&self, idx: usize) -> &Class;
}

pub struct BasicClassProvider {
    unnamed_count: usize,
    count_str: String,
    pub classes: Vec<Class>,

    pub subclass_dict: Vec<HashSet<usize>>,
    pub superclass_dict: Vec<HashSet<usize>>,
}
impl Default for BasicClassProvider {
    fn default() -> Self {
        Self::new()
    }
}
impl BasicClassProvider {
    pub fn new() -> Self {
        Self {
            unnamed_count: 0,
            count_str: String::from("0"),

            classes: Vec::new(),
            subclass_dict: Vec::new(),
            superclass_dict: Vec::new(),
        }
    }
}

impl ClassProvider for BasicClassProvider {
    fn unnamed(&mut self, id: Option<&str>, ty: &'static str) -> usize {
        let id: &str = if let Some(id) = id {
            id
        } else {
            &self.count_str
        };

        let class_ref = ClassRef::Unnamed(&id, ty);
        if let Some(idx) = self.classes.iter().position(|clazz| class_ref.eq(clazz)) {
            idx
        } else {
            let new_class = Class::Unnamed(self.unnamed_count.to_string().into(), ty);
            self.unnamed_count += 1;

            let out = self.classes.len();
            self.classes.push(new_class);
            self.subclass_dict.push(HashSet::from([out]));
            self.superclass_dict.push(HashSet::from([out]));
            self.count_str = self.unnamed_count.to_string();

            out
        }
    }

    fn named(&mut self, id: &str) -> usize {
        let class_ref = ClassRef::Named(&id);
        if let Some(idx) = self.classes.iter().position(|clazz| class_ref.eq(clazz)) {
            idx
        } else {
            let new_class = Class::Named(id.to_string().into());
            let out = self.classes.len();
            self.classes.push(new_class);
            self.subclass_dict.push(HashSet::from([out]));
            self.superclass_dict.push(HashSet::from([out]));
            out
        }
    }

    fn add_subclass(&mut self, class: usize, subclass: usize) {
        self.subclass_dict[class].insert(subclass);
        let new_ones = self.subclass_dict[subclass].clone();
        self.subclass_dict[class].extend(&new_ones);

        for inv in &self.superclass_dict[class] {
            self.subclass_dict[*inv].extend(&new_ones);
        }

        self.superclass_dict[subclass].insert(class);
        let new_ones = self.superclass_dict[class].clone();
        self.superclass_dict[subclass].extend(&new_ones);

        for inv in &self.subclass_dict[class] {
            self.superclass_dict[*inv].extend(&new_ones);
        }
    }

    fn class(&self, idx: usize) -> &Class {
        &self.classes[idx]
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use hashbrown::HashSet;

    #[test]
    fn subclass_works() {
        let mut provider = BasicClassProvider::new();

        let c1 = provider.named("1");
        let c2 = provider.named("2");
        let c3 = provider.named("3");
        let c4 = provider.named("4");
        let c5 = provider.named("5");
        let c6 = provider.named("6");

        provider.add_subclass(c1, c2);
        provider.add_subclass(c1, c3);
        provider.add_subclass(c4, c5);
        provider.add_subclass(c6, c4);

        assert_eq!(provider.subclass_dict[0], HashSet::from_iter(vec![1, 2, 0]));
        assert_eq!(provider.subclass_dict[3], HashSet::from_iter(vec![4, 3]));
        // assert_eq!(provider.superclass_dict[1], HashSet::from_iter(vec![0]));
        // assert_eq!(provider.superclass_dict[2], HashSet::from_iter(vec![0]));
        // assert_eq!(provider.superclass_dict[4], HashSet::from_iter(vec![3]));

        provider.add_subclass(c4, c1);
        assert_eq!(
            provider.subclass_dict[3],
            HashSet::from_iter(vec![0, 1, 2, 3, 4])
        );
        assert_eq!(
            provider.subclass_dict[5],
            HashSet::from_iter(vec![0, 1, 2, 3, 4, 5])
        );
        // assert_eq!(provider.superclass_dict[0], HashSet::from_iter(vec![3]));
        // assert_eq!(provider.superclass_dict[1], HashSet::from_iter(vec![0, 3]));
    }
}
