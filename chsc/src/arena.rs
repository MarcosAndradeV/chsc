pub struct Arena<T> {
    pub inner: std::cell::RefCell<Vec<T>>,
}

const ARENA_DEFAULT_CAP: usize = 1024;

impl<T> Arena<T> {
    pub fn new() -> Self {
        let capacity = ARENA_DEFAULT_CAP / std::mem::size_of::<T>();
        Self {
            inner: Vec::with_capacity(capacity).into(),
        }
    }
    pub fn alloc(&self, value: T) -> &T {
        let mut ref_cell = self.inner.borrow_mut();
        let len = ref_cell.len();
        assert!(len < ref_cell.capacity());
        ref_cell.push(value);
        unsafe { &*ref_cell.as_ptr().add(len) }
    }
}
