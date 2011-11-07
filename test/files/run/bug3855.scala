object Test {
  def byval[A](a: => A) = a
  def closure[A](f: () => A) = f()
  
  def f1(s: String) = {
    var n = try { s.toInt } catch { case _ => 1 }
    byval(n)
  }
  def f2(s: String) = {
    var n = try { s.toInt } catch { case _ => 1 }
    closure(() => n)
  }

  def main(args: Array[String]) = {
    val sum = f1("12") + f2("the witch is dead")
    assert(sum == 13)
  }
}
