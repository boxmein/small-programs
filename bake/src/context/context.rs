use crate::traits::{Context, Logger};

pub struct DefaultContext<L: Logger> {
    pub logger: L,
}

impl<L: Logger> Context for DefaultContext<L> {
    type LoggerType = L;
    fn get_logger(&self) -> &Self::LoggerType {
        &self.logger
    }
}
