//> using options -Xreporter:scala.tools.partest.nest.PlainReporter

trait A {
  def x: String
}

class B[+T] extends A {
  def y(t: T): Unit = ()
}
