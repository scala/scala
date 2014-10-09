object O {
  @inline def f(a: Int, b: Int) = a + b

  // this test should not generate any inliner warnings

  def g(a: Int, b: Int) =
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b) +
    f(a, b)

  def h = Map(
    "a" -> 0, "b" -> 1, "c" -> 2, "d" -> 3, "e" -> 4,
    "a" -> 0, "b" -> 1, "c" -> 2, "d" -> 3, "e" -> 4,
    "a" -> 0, "b" -> 1, "c" -> 2, "d" -> 3, "e" -> 4,
    "a" -> 0, "b" -> 1, "c" -> 2, "d" -> 3, "e" -> 4,
    "a" -> 0, "b" -> 1, "c" -> 2, "d" -> 3, "e" -> 4,
    "a" -> 0, "b" -> 1, "c" -> 2, "d" -> 3, "e" -> 4,
    "f" -> 5, "g" -> 6, "h" -> 7, "i" -> 8)
}
