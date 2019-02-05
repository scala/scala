object Test {
    def main(args: Array[String]): Unit = {
        class X { override def equals(x: Any) = throw new Exception("shouldn't be called") }
        case class C(x: X, i: Int)
        val x = new X

        C(x, 1) == C(x, 2)
    }
}
