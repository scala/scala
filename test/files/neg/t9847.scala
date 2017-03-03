
trait T {

  def f(): Unit = 42
  def g = (42: Unit)
  def h = {
    1
  + 1
  }
  def hh(): Unit = {
    1
  + 1
  }
  def i(): Unit = {
    val x = 1
    x + 1
  }
  def x = 42
  def j(): Unit = x + 1

  class C { 42 }
  class D { 42 ; 17 }
}
