import exception
import gleeunit
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn rescue_ok_test() {
  assert Ok(1) == exception.rescue(fn() { 1 })
}

pub fn rescue_errored_test() {
  let assert Error(exception.Errored(_)) = exception.rescue(fn() { panic })
}

@external(erlang, "erlang", "throw")
@external(javascript, "./exception_test_ffi.mjs", "throw_")
fn throw(a: whatever) -> Nil

pub fn rescue_thrown_test() {
  let assert Error(exception.Thrown(_)) =
    exception.rescue(fn() { throw("123") })
}

@external(erlang, "erlang", "exit")
@external(javascript, "./exception_test_ffi.mjs", "throw_")
fn exit(a: whatever) -> Nil

@target(erlang)
pub fn rescue_exited_test() {
  let assert Error(exception.Exited(_)) = exception.rescue(fn() { exit("123") })
}

pub fn defer_ok_test() {
  reset()

  let assert 2 = {
    use <- exception.defer(fn() { append("1") })
    use <- exception.defer(fn() { append("2") })
    use <- exception.defer(fn() { append("3") })
    2
  }

  assert "321" == read()
}

pub fn defer_crash_test() {
  reset()

  let assert Error(_) =
    exception.rescue(fn() {
      use <- exception.defer(fn() { append("3") })
      use <- exception.defer(fn() { append("2") })
      append("1")
      panic
    })

  assert "123" == read()
}

pub fn on_crash_ok_test() {
  reset()

  let assert 2 = {
    use <- exception.on_crash(fn() { append("1") })
    use <- exception.on_crash(fn() { append("2") })
    use <- exception.on_crash(fn() { append("3") })
    2
  }

  assert "" == read()
}

pub fn on_crash_crash_test() {
  reset()

  let assert Error(_) =
    exception.rescue(fn() {
      use <- exception.on_crash(fn() { append("3") })
      use <- exception.on_crash(fn() { append("2") })
      append("1")
      panic
    })

  assert "123" == read()
}

const test_file = "tmp.txt"

fn reset() -> Nil {
  let assert Ok(_) = simplifile.write(test_file, "")
  Nil
}

fn append(text: String) -> Nil {
  let assert Ok(_) = simplifile.append(test_file, text)
  Nil
}

fn read() -> String {
  let assert Ok(text) = simplifile.read(test_file)
  text
}
