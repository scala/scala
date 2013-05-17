class Test {
  def combine = this match {
    case that if that eq this => this // just return this
    case that: Test2 =>
      println(that)
      this
    case _ => sys.error("meh")
  }
}

class Test2 extends Test
