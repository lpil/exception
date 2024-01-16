import gleeunit
import exception
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn rescue_ok_test() {
  let assert Ok(1) = exception.rescue(fn() { 1 })
}

pub fn rescue_error_test() {
  let assert Error(_) = exception.rescue(fn() { panic })
}

pub fn defer_ok_test() {
  reset()

  let assert 2 = {
    use <- exception.defer(fn() { append("1") })
    use <- exception.defer(fn() { append("2") })
    use <- exception.defer(fn() { append("3") })
    2
  }

  let assert "321" = read()
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

  let assert "123" = read()
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
