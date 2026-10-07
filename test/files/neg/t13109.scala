//> using options -feature -Werror

class A {
  val b = new AnyRef { def x: Int = 2 }

  def f1 = b.x // warn

  def f2 = {
    import b._
    x // also expect warn but not report
  }
}
