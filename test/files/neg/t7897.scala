package p0 {
  class Single(val x: Any) extends AnyRef with Product1[String] {
    private def s = "" + x
    override def canEqual(x: Any) = this eq x.asInstanceOf[AnyRef]
    def isEmpty = false
    def get = this
    def _1 = s + " only"

    override def toString = s"Single(${_1})"
  }

  object Single {
    def unapply(x: Any): Single = new Single(x)
  }
}
object Test {
  def main(args: Array[String]): Unit = {
    "catdog" match {
      case p0.Single(x) => println(s"`$x` has ${x.length} chars")
      case x            => println("fail: " + x)
    }
  }
}
