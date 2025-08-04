
#[derive(Default)]
pub struct Arena<T> {
    pub inner: std::cell::RefCell<Vec<T>>,
}

impl<T> Arena<T> {
    pub fn new() -> Self {
        Self {
            inner: Vec::new().into(),
        }
    }
    pub fn alloc(&self, value: T) -> &T {
        let mut ref_cell = self.inner.borrow_mut();
        let len = ref_cell.len();
        ref_cell.push(value);
        unsafe { &*ref_cell.as_ptr().add(len) }
    }
}
