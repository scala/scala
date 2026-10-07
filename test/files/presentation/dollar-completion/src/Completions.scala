package test

case class C1(x: Int) {
  // Filter out `def copy$default$1: Int`
  /*_*/
}

trait T {
  println("hello")

  // Filter out `$init$`
  /*_*/
}

class C2 {
  def `$` = 1
  def `dollar$` = 2
  def `$var` = 3

  // Include explicit dollar methods
  /*_*/
}
