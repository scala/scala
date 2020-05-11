
class C(a: Int, b: Int)

trait T {
  def c1 = new C(a = 42, b = 17)
  def c2 = new C(a = 42, b = 17, c = 5)
  def c3 = new C(b = 42, a = 17, c = 5)
  def c4 = new C(b = 42, a = 17, 5)
  def c5 = new C(a = 42, c = 17)
  def c6 = new C(a = 42, c = 17, b = 5)
  def c7 = new C(c = 42, a = 17, b = 5)
  def c8 = new C(42, 17, b = 5)
  def c9 = new C(a = 42, c = 17, d = 3, b = 5)
  def c0 = new C(42, 17, d = 3, c = 5)
}

trait X {
  trait Upper
  def f0() = 42
  def f1(i: Int) = 42
  def f2(i: Int, j: Int) = 42
  def f6(i: Int, j: Int, k: Int, l: Int, m: Int, n: Int) = 42
  def f12(i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int, r: Int, s: Int, t: Int) = 42
  def max(x: Any) = 42
  def maxi[A](x: A) = 42
  def mini[A <: Upper](x: A) = 42
  def moxi(x: AnyRef) = 42
  def movi(x: Object) = 42

  def g() = {
    f0(1)
    f1(1, 2)
    f1(1, 2, 3)
    f1(1, 2, 3, 4)
    f1(1, j = 2, 3, 4)
    f1(1, j = 2, k = 3, 4)
    f2(k = 1, i = 2, j = 3)
    f6(1, 2, 3, 4, 5, 6, 7)
    f6(1, 2, 3, 4, 5, 6, 7, 8)
    f12(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    // confirm auto-tupling
    max(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    maxi(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    moxi(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    movi(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    mini(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    // clarify auto-tupling failure
    max(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
    maxi(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
    moxi(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
    movi(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
    mini(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
    ()
  }
}

trait Y {
  def f[A](a: A)       = ???
  def g(x: Any)        = ???
  def t(t: (Int, Int)) = ???

  def x = f(42, 27, 17)
  def y = g(42, 27, 17)
  def z = t(42, 27, 17)
}

trait Z {
  def f(a: Int = 1, b: String = "") = b * a

  def x = f(c = "hello, world")
}

object app {
  def test(): Unit = {
    val x = null.asInstanceOf[X]
    x f0 ()
    x.f0(())
  }
  def workaround(): Unit = {
    import language.postfixOps
    val x = null.asInstanceOf[X]
    x f0
  }
}
