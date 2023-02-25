use std::{
    borrow::Borrow,
    ops::{Deref, Range},
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

#[derive(Clone, Debug, Copy)]
pub enum JsonRef<'a> {
    Invalid,
    Null,
    KV(&'a ObjMember),
    Bool(bool),
    Str(&'a String),
    Num(f64),
    Array(&'a [Spanned<Json>]),
    Object(ObjRef<'a>),
}

impl<'a> From<&'a Json> for JsonRef<'a> {
    fn from(x: &'a Json) -> Self {
        match x {
            Json::Invalid => Self::Invalid,
            Json::Null => Self::Null,
            Json::Bool(x) => Self::Bool(*x),
            Json::Str(y) => Self::Str(&y),
            Json::Num(x) => Self::Num(*x),
            Json::Array(x) => Self::Array(&x),
            Json::Object(x) => Self::Object(ObjRef(&x)),
        }
    }
}

#[allow(unused)]
impl Json {
    pub fn as_ref<'a>(&'a self) -> JsonRef<'a> {
        self.into()
    }
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
impl<'a> JsonRef<'a> {
    pub fn as_bool(self) -> Option<bool> {
        match self {
            JsonRef::Bool(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_str(self) -> Option<&'a str> {
        match self {
            JsonRef::Str(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_num(self) -> Option<f64> {
        match self {
            JsonRef::Num(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_arr(self) -> Option<&'a [Spanned<Json>]> {
        match self {
            JsonRef::Array(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_obj(self) -> Option<ObjRef<'a>> {
        match self {
            JsonRef::Object(x) => Some(x),
            _ => None,
        }
    }
}

impl Spanned<Json> {
    pub fn iter<'a>(&'a self) -> JsonIter<'a> {
        JsonIter {
            stack: vec![self.map_ref(|x| JsonRef::from(x))],
        }
    }
}

pub struct JsonIter<'a> {
    stack: Vec<Spanned<JsonRef<'a>>>,
}

impl<'a> Iterator for JsonIter<'a>
where
    Self: 'a,
{
    type Item = Spanned<JsonRef<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.stack.pop() {
            match item.0 {
                JsonRef::Array(s) => {
                    self.stack
                        .extend(s.iter().rev().map(|x| x.map_ref(Json::as_ref)));
                }
                JsonRef::Object(ObjRef(s)) => {
                    self.stack
                        .extend(s.into_iter().map(|y| y.map_ref(|x| JsonRef::KV(x))));
                }
                JsonRef::KV(mem) => {
                    let (k, v) = (&mem.0, &mem.1);
                    self.stack.push(k.map_ref(|x| JsonRef::Str(x)));
                    self.stack.push(v.map_ref(Json::as_ref));
                }
                _ => {}
            };

            Some(item)
        } else {
            None
        }
    }
}
