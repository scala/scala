object Utils1 {
  @inline def blackMagic1(n: Int, z: Int): Int = {
    if (n < z)
      n * n
    else
      n
  }
}

object Utils2 {
  @inline def applyToInt(i: Int, f: Int => Int) =
    f(i)
  @inline def blackMagic2(n: Int, z: Int): Int = {
    if (n < z)
      n * n
    else
      n
  }
}