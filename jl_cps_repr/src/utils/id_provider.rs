#[derive(Default)]
pub struct IdProvider {
    last_id: usize,
}

impl IdProvider {
    pub fn next(&mut self) -> usize {
        self.last_id += 1;
        self.last_id
    }
}
