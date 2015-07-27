package p
class C {
  @inline final def f(x: Int): Long = 10L / (if (x < 0) -2 else 2)
  @inline final def g(x: Int): Long = 3000L / (if (x < 0) -300 else 300)
}
