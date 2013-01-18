object Test {
  type A = Int
  // Emits the implementation restriction but then proceeds to crash
  // when creating the Foo.unapply.
  case class Foo(a: A, b: A, c: A, d: A, e: A, f: A, g: A, h: A, i: A, j: A, k: A, l: A, m: A, n: A, o: A, p: A, q: A, r: A, s: A, t: A, u: A, v: A, w: A, x: A, y: A, Z: A)
}
