object Utils_1 {
  @inline def blackMagic(n: Int, z: Int): Int = {
    if (n < z)
      n * n
    else
      n
  }
  @inline def applyToInt(i: Int, f: Int => Int): Int =
    f(i)
}