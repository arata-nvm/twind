use std::ops::Deref;

#[derive(Debug, Clone)]
pub struct Environment<K, V>(Vec<(K, V)>);

impl<K: Clone + PartialEq, V: Clone> Environment<K, V> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn expand(&mut self, key: K, value: V) {
        self.0.push((key, value));
    }

    pub fn expanded(&self, key: K, value: V) -> Self {
        let mut newenv = self.clone();
        newenv.expand(key, value);
        newenv
    }

    pub fn lookup(&self, key_to_find: &K) -> Option<V> {
        self.0
            .iter()
            .rev()
            .find(|(key, _)| key == key_to_find)
            .map(|(_, value)| value.clone())
    }
}

impl<K, V> Deref for Environment<K, V> {
    type Target = Vec<(K, V)>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
