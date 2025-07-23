use std::marker::PhantomData;

#[derive(Default)]
pub struct Arena<'arena, T> {
    phantom: PhantomData<&'arena T>,
    pub inner: std::cell::RefCell<Vec<T>>,
}

const ARENA_DEFAULT_CAP: usize = 1024;

impl<'arena, T> Arena<'arena, T> {
    pub fn new() -> Self {
        let capacity = ARENA_DEFAULT_CAP / std::mem::size_of::<T>();
        Self {
            phantom: PhantomData,
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
