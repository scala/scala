trait Main {
  trait A {
    type B
  }
  trait C {
    def c(a: A, x: Int = 0)(b: a.B)
  }
  def c: C
  def d(a: A, x: Int = 0)(b: a.B)

  def ok1(a: A)(b: a.B) = c.c(a, 42)(b)
  def ok2(a: A)(b: a.B) = d(a)(b)

  def fail(a: A)(b: a.B) = c.c(a)(b)
}
