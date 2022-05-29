use std::{
    fs,
    time::{SystemTime, UNIX_EPOCH},
};

use crate::interpreter::{NativeFnValue, RoxValue};

pub const CLOCK_NATIVE_FN: NativeFnValue = NativeFnValue {
    name: "clock",
    arity: 0,
    native_fn: |_args: Vec<RoxValue>| {
        Ok(RoxValue::Number(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_millis() as f64,
        ))
    },
};

pub const READ_FILE_NATIVE_FN: NativeFnValue = NativeFnValue {
    name: "read_file",
    arity: 1,
    native_fn: |args: Vec<RoxValue>| {
        let filename = args[0].clone().to_rox_string()?;

        let content = fs::read_to_string(filename).expect("Something went wrong reading the file");
        Ok(RoxValue::String(content))
    },
};
