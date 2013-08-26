trait Main {
  trait A {
    type B
    def b: B
  }
  trait C {
    def c(a: A, x: Int = 0)(b: => a.B, bs: a.B*)
    def d(a: A = null, x: Int = 0)(b1: => a.B = a.b, b2: a.B = a.b)
  }
  def c: C
  def ok(a: A)(b: a.B) = c.c(a, 42)(b)
  def fail(a: A)(b: a.B) = c.c(a)(b)
  def fail2(a: A)(b: a.B) = c.c(a)(b, b)
  def fail3(a: A)(b: a.B) = c.c(a)(b, Seq[a.B](b): _*)

  def fail4(a: A)(b: a.B) = c.d(a)()
  def fail5(a: A)(b: a.B) = c.d(a)(b1 = a.b)
  def fail6(a: A)(b: a.B) = c.d(a)(b2 = a.b)
  def fail7(a: A)(b: a.B) = c.d()()
}
