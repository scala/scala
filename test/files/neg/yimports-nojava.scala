//> using options -Yimports:scala,scala.Predef

trait T {
  def f() = println("hello, world!")
  def g() = new Integer(42)
  def sleep() = Thread.sleep(42000L)
}
