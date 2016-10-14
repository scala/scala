package p1 {
  object O {
    private case class N(a: Any)
    lazy val x: AnyRef = N
    lazy val y: AnyRef = new { assert(N != null) }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    p1.O.x
    p1.O.y
  }
}
