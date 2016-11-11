
package shifty

object X {
  @inline def f(i: Int): Int = i >> 1
  @inline def g(i: Int): Int = i >>> 1
}
