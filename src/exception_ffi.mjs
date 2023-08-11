import { Ok, Error as GError } from "./gleam.mjs";

export function rescue(f) {
  try {
    return new Ok(f());
  } catch (e) {
    return new GError(e);
  }
}

export function defer(cleanup, body) {
  try {
    return body();
  } finally {
    cleanup();
  }
}
