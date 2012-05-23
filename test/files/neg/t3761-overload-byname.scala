object t {
  def m1(x: => AnyVal, s: String) = 0
  def m1(x: => Int, s: Object) = 1

  def m2(x: => Any, s: String) = 0
  def m2(x: => Int, s: Object) = 1


  m1(1, "")
  m1(1d, "")
  m2(1, "")
  m2("", "")
}
