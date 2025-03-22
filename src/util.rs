use super::*;
use core::ops::{Deref, DerefMut};

#[derive(Clone, Debug, PartialEq)]
pub struct SrcNode<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> Deref for SrcNode<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> DerefMut for SrcNode<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<T> SrcNode<T> {
    pub fn new(inner: T, span: Span) -> Self {
        Self { inner, span }
    }
}
