use std::{
    borrow::Borrow,
    ops::{Deref, Range},
};

use enum_methods::{EnumIntoGetters, EnumIsA, EnumToGetters};

#[derive(Debug, Clone)]
pub struct Spanned<T>(pub T, pub Range<usize>);
impl<T> Default for Spanned<T>
where
    T: Default,
{
    fn default() -> Self {
        Self(T::default(), 0..1)
    }
}
impl<T> Spanned<T> {
    pub fn map<O>(self, f: impl Fn(T) -> O) -> Spanned<O> {
        let v = f(self.0);
        Spanned(v, self.1)
    }
    pub fn map_ref<'a, O: 'a>(&'a self, f: impl Fn(&'a T) -> O) -> Spanned<O> {
        let v = f(&self.0);
        Spanned(v, self.1.clone())
    }
    pub fn as_ref<'a>(&'a self) -> Spanned<&'a T> {
        Spanned(&self.0, self.1.clone())
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
    pub fn value_mut(&mut self) -> &mut T {
        &mut self.0
    }
    pub fn span(&self) -> &Range<usize> {
        &self.1
    }
}

pub type ObjMember = Option<(Spanned<String>, Spanned<Json>)>;

#[derive(Clone, Debug, PartialEq)]
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
            .flat_map(|x| x.value())
            .find(|x| x.0.value() == s)
            .map(|x| &x.1)
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
            .flat_map(|x| x.value())
            .find(|x| x.0.value() == s)
            .map(|x| &x.1)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Json {
    Invalid,
    Null,
    Bool(bool),
    Str(String),
    Num(f64),
    Array(Vec<Spanned<Json>>),
    Object(Obj),
}

#[derive(Clone, Debug, PartialEq, EnumIntoGetters, EnumIsA, EnumToGetters)]
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

impl JsonToken {
    pub fn ty(&self) -> &'static str {
        match self {
            JsonToken::Invalid => "invalid",
            JsonToken::Null => "null",
            JsonToken::KV(_, _) => "kv",
            JsonToken::Bool(_) => "bool",
            JsonToken::Str(_) => "str",
            JsonToken::Num(_) => "num",
            JsonToken::Array(_) => "array",
            JsonToken::Obj(_) => "obj",
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
