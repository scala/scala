
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
  def f0() = 42
  def f1(i: Int) = 42
  def f2(i: Int, j: Int) = 42
  def f6(i: Int, j: Int, k: Int, l: Int, m: Int, n: Int) = 42
  def f12(i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: Int, p: Int, q: Int, r: Int, s: Int, t: Int) = 42

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
    ()
  }
}
