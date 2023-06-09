#[derive(Debug, Clone)]
pub struct Environment<T>(Vec<(String, T)>);

impl<T: Clone> Environment<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn expand(&mut self, name: String, value: T) {
        self.0.push((name, value));
    }

    pub fn expanded(&self, name: String, value: T) -> Self {
        let mut newenv = self.clone();
        newenv.expand(name, value);
        newenv
    }

    pub fn lookup(&self, name: &String) -> Option<T> {
        self.0
            .iter()
            .rev()
            .find(|(var_name, _)| var_name == name)
            .map(|(_, value)| value.clone())
    }
}
