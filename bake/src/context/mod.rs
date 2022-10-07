mod context;
mod logger;

use crate::traits::Context;

pub fn get_context() -> impl Context {
    context::DefaultContext {
        logger: logger::StdoutLogger {},
    }
}
