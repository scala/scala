package crasher {
  class Z[@specialized A, @specialized(AnyRef) B](var a: A, var b: B) {
    override def toString = "" + ((a, b))
  }
  object O {
    def apply[@specialized A, @specialized(AnyRef) B](a0: A, b0: B) = new Z(a0, b0)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    println(crasher.O("a", 2))
    println(crasher.O(2, "a"))
  }
}
