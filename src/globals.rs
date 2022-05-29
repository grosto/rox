use std::time::{SystemTime, UNIX_EPOCH};

use crate::interpreter::{Callable, EvaluationResult, RoxValue, WrappedEnvironment};

#[derive(Clone, Debug)]
pub struct ClockNativeFn {}

impl Callable for ClockNativeFn {
    fn arity(&self) -> u16 {
        0
    }

    fn call(&self, _env: WrappedEnvironment, _arguments: Vec<RoxValue>) -> EvaluationResult {
        Ok(RoxValue::Number(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_millis() as f64,
        ))
    }

    fn name(&self) -> String {
        "clock".into()
    }
}

pub const CLOCK_NATIVE_FN: ClockNativeFn = ClockNativeFn {};
