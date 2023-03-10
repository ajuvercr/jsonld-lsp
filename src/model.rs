use std::{
    borrow::Borrow,
    ops::{Deref, Index, Range},
};

#[derive(Debug, Clone)]
pub struct Spanned<T>(pub T, pub Range<usize>);
impl<T> Spanned<T> {
    pub fn map<O>(self, f: impl Fn(T) -> O) -> Spanned<O> {
        let v = f(self.0);
        Spanned(v, self.1)
    }
    pub fn map_ref<'a, O: 'a>(&'a self, f: impl Fn(&'a T) -> O) -> Spanned<O> {
        let v = f(&self.0);
        Spanned(v, self.1.clone())
    }
}
impl<T> Spanned<Option<T>> {
    pub fn transpose(self) -> Option<Spanned<T>> {
        self.0.map(|inner| Spanned(inner, self.1))
    }
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(&other.0)
    }
}
impl<T: std::hash::Hash> std::hash::Hash for Spanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}
impl<T: PartialEq> Eq for Spanned<T> {}

pub fn spanned<T>(t: T, span: Range<usize>) -> Spanned<T> {
    Spanned(t, span)
}

impl Borrow<str> for Spanned<String> {
    #[inline]
    fn borrow(&self) -> &str {
        &self[..]
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> Spanned<T> {
    pub fn into_value(self) -> T {
        self.0
    }
    pub fn into_span(self) -> Range<usize> {
        self.1
    }
    pub fn value(&self) -> &T {
        &self.0
    }
    pub fn span(&self) -> &Range<usize> {
        &self.1
    }
}

pub type ObjMember = (Spanned<String>, Spanned<Json>);

#[derive(Clone, Debug)]
pub struct Obj(pub Vec<Spanned<ObjMember>>);

impl Deref for Obj {
    type Target = Vec<Spanned<ObjMember>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl Obj {
    pub fn get<'a, S>(&'a self, s: S) -> Option<&'a Spanned<Json>>
    where
        for<'b> &'b String: PartialEq<S>,
    {
        self.0
            .iter()
            .find(|x| x.value().0.value() == s)
            .map(|x| &x.value().1)
    }
}

#[derive(Clone, Debug, Copy)]
pub struct ObjRef<'a>(pub &'a [Spanned<ObjMember>]);

impl<'a> Deref for ObjRef<'a> {
    type Target = [Spanned<ObjMember>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<'a> ObjRef<'a> {
    pub fn get<S>(&self, s: S) -> Option<&'a Spanned<Json>>
    where
        for<'b> &'b String: PartialEq<S>,
    {
        self.0
            .into_iter()
            .find(|x| x.value().0.value() == s)
            .map(|x| &x.value().1)
    }
}

#[derive(Clone, Debug)]
pub enum Json {
    Invalid,
    Null,
    Bool(bool),
    Str(String),
    Num(f64),
    Array(Vec<Spanned<Json>>),
    Object(Obj),
}

#[derive(Clone, Debug)]
pub enum JsonToken {
    Invalid,
    Null,
    KV(Spanned<String>, usize),
    Bool(bool),
    Str(String),
    Num(f64),
    Array(Vec<usize>),
    Obj(Vec<usize>),
}

#[derive(Clone, Debug)]
pub struct ParentingSystem {
    objects: Vec<Spanned<JsonToken>>,
    parents: Vec<usize>,
}

impl Index<usize> for ParentingSystem {
    type Output = Spanned<JsonToken>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.objects[index]
    }
}

impl ParentingSystem {
    fn add(&mut self, json: Spanned<Json>, parent: usize) -> usize {
        let out = self.parents.len();

        self.parents.push(parent);

        match json.0 {
            Json::Invalid => self.objects.push(json.map(|_| JsonToken::Invalid)),
            Json::Null => self.objects.push(json.map(|_| JsonToken::Null)),
            Json::Bool(x) => self.objects.push(json.map(|_| JsonToken::Bool(x))),
            Json::Str(x) => self.objects.push(Spanned(JsonToken::Str(x), json.1)),
            Json::Num(x) => self.objects.push(json.map(|_| JsonToken::Num(x))),
            Json::Array(ar) => {
                self.objects.push(Spanned(JsonToken::Invalid, 0..0));
                let children = ar.into_iter().map(|x| self.add(x, out)).collect();
                self.objects[out] = Spanned(JsonToken::Array(children), json.1);
            }
            Json::Object(obj) => {
                self.objects.push(Spanned(JsonToken::Invalid, 0..0));
                let children: Vec<_> = obj
                    .0
                    .into_iter()
                    .map(|Spanned((k, v), span)| {
                        let kv_idx = self.parents.len();
                        self.parents.push(out);
                        self.objects.push(Spanned(JsonToken::Invalid, 0..0));

                        let v = self.add(v, kv_idx);
                        self.objects[kv_idx] = Spanned(JsonToken::KV(k, v), span);

                        kv_idx
                    })
                    .collect();
                self.objects[out] = Spanned(JsonToken::Obj(children), json.1);
            }
        }

        out
    }
    pub fn from_json(json: Spanned<Json>) -> Self {
        let mut this = Self {
            objects: Vec::new(),
            parents: Vec::new(),
        };
        this.add(json, 0);
        this
    }

    pub fn parent(&self, idx: usize) -> Option<(usize, &Spanned<JsonToken>)> {
        if idx == 0 {
            None
        } else {
            let ids = self.parents[idx];
            Some((ids, &self.objects[ids]))
        }
    }
    pub fn parent_iter<'a>(&'a self, idx: usize) -> ParentIter<'a> {
        ParentIter { parents: self, idx }
    }
    pub fn iter<'a>(&'a self) -> impl Iterator<Item = (usize, &'a Spanned<JsonToken>)> {
        self.objects.iter().enumerate()
    }
}

pub struct ParentIter<'a> {
    parents: &'a ParentingSystem,
    idx: usize,
}
impl<'a> Iterator for ParentIter<'a> {
    type Item = (usize, &'a Spanned<JsonToken>);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((idx, o)) = self.parents.parent(self.idx) {
            self.idx = idx;
            Some((idx, o))
        } else {
            None
        }
    }
}

#[allow(unused)]
impl Json {
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Json::Bool(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Json::Str(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_num(&self) -> Option<f64> {
        match self {
            Json::Num(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_arr(&self) -> Option<&[Spanned<Json>]> {
        match self {
            Json::Array(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_obj(&self) -> Option<&Obj> {
        match self {
            Json::Object(x) => Some(x),
            _ => None,
        }
    }
}

#[allow(unused)]
impl JsonToken {
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            JsonToken::Bool(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            JsonToken::Str(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_num(&self) -> Option<f64> {
        match self {
            JsonToken::Num(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_arr(&self) -> Option<&[usize]> {
        match self {
            JsonToken::Array(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_kv(&self) -> Option<(&Spanned<String>, usize)> {
        match self {
            JsonToken::KV(x, y) => Some((&x, *y)),
            _ => None,
        }
    }

    pub fn as_obj(&self) -> Option<&[usize]> {
        match self {
            JsonToken::Obj(x) => Some(x),
            _ => None,
        }
    }
}
