use std::any::Any;
use std::cell::RefCell;
use std::sync::Arc;

thread_local! {
    pub static BAR: RefCell<ThreadLocalStorage> = RefCell::new(ThreadLocalStorage {
        value: None
    });
}

pub struct ThreadLocalStorage {
    pub value: Option<Arc<Box<dyn Any>>>,
}

pub fn provide_context<T, F>(value: T, func: F) -> ()
where
    T: Clone + 'static,
    F: Fn() -> (),
{
    let newval: Box<dyn Any> = Box::new(value.clone());
    let newval: Arc<Box<dyn Any>> = Arc::new(newval);
    BAR.with(|bar| {
        (*bar.borrow_mut()).value = Some(newval);
    });
    func();
}

pub fn use_context<T, F>(func: F) -> ()
where
    T: Clone + 'static,
    F: Fn(Arc<Box<T>>) -> (),
{
    BAR.with(|storage| {
        let storage: &ThreadLocalStorage = &storage.borrow();
        let value: &Option<Arc<Box<dyn Any>>> = &storage.value;
        assert!(value.is_some());
        let value: &Arc<Box<dyn Any>> = &value.as_ref().unwrap();
        let value: Arc<Box<dyn Any>> = value.clone();
        let value: &Arc<Box<T>> = value.downcast_ref::<Arc<Box<T>>>().unwrap();
        let value: Arc<Box<T>> = (*value).clone();

        func(value);
    });
}

#[cfg(test)]
mod tests {
    #[test]
    fn works() {
        use super::*;

        let value = "hello";

        provide_context(value.to_string(), || {
            use_context(|val: Arc<Box<String>>| {
                assert_eq!(**val, value.to_string());
            });
        });
    }
}
