object Test {
  //Were affected by SI-6306
  def f[A](a: =>A) = println(a.toString)
  def f1[A <: AnyVal](a: =>A) = println(a.toString)
  def f1a[A <: AnyVal](a: =>A) = println(a.##)
  def f2[A <: AnyRef](a: =>A) = println(a.toString)
  def f2a[A <: String](a: =>A) = println(a.toString)
  //Works
  def f3[A](a: =>Seq[A]) = println(a.toString)

  def foo() = println(2)
  def client(f: () => Unit) = {f(); f()}
  def attempt2() {
    val bar: () => Unit = foo _
    //The code causing SI-6306 was supposed to optimize code like this:
    client(() => bar ())
    //to:
    client(bar)
  }
  def main(args: Array[String]) {
    attempt2()
    f3(Seq(1))
    f3(Seq())
    f("")
    f((1).toString)
    f((1).##)
    f1((1).##)
    f2((1).toString)
    f2a((1).toString)
  }
}

// vim: set ts=8 sw=2 et:
