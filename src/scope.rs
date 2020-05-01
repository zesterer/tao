#[derive(Clone)]
pub enum Scope<'a, K, V, F> {
    Root(F),
    Local(K, V, &'a Self),
}

impl<'a, K, V, F> Scope<'a, K, V, F> {
    pub fn root(f: F) -> Self {
        Scope::Root(f)
    }

    pub fn with<'b>(&'b self, key: K, val: V) -> Scope<'b, K, V, F> where 'a: 'b {
        Scope::Local(key, val, self)
    }

    pub fn get(&self, key: &K) -> Option<&V>
        where K: PartialEq<K>, F: Fn(&K) -> Option<&'a V>
    {
        match self {
            Scope::Local(local_key, val, parent) => if key == local_key {
                Some(val)
            } else {
                parent.get(key)
            },
            Scope::Root(f) => f(key),
        }
    }
}
