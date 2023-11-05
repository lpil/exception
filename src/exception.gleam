import gleam/dynamic.{type Dynamic}

// TODO: implement 
// TODO: test 
// TODO: document
/// This function will catch any crash and convert it into a result rather than
/// crashing the process.
///
/// You should ideally never use this function! Exceptions are not flow control
/// in Gleam, a result type should be used instead. This function is only if you
/// need to perform some cleanup when a crash occurs, and then you should favour
/// `defer` if possible.
///
@external(erlang, "exception_ffi", "rescue")
@external(javascript, "./exception_ffi.mjs", "rescue")
pub fn rescue(body: fn() -> a) -> Result(a, Dynamic)

// TODO: implement 
// TODO: test 
/// This function will run a cleanup function after the given body function, even
/// if the body function crashes.
///
/// You should ideally never use this function! Exceptions are not flow control
/// in Gleam, a result type should be used instead. This function is only if you
/// need to perform some cleanup when a crash occurs.
///
/// # Examples
/// 
/// ```gleam
/// pub fn run_with_lock(f: fn() -> a) -> a {
///   let lock = acquire()
///   use <- defer(fn() { release(lock) })
///   f()
/// }
/// ```
/// 
///
@external(erlang, "exception_ffi", "defer")
@external(javascript, "./exception_ffi.mjs", "defer")
pub fn defer(cleanup: fn() -> b, body: fn() -> a) -> a
