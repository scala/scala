object Test {
  def main(args: Array[String]): Unit = {
    val o1 = Outer(5)
    o1 match {
      case o @ Outer(_) =>
        val i = new o.Inner
    }
    o1 match {
      case o : Outer =>
        val i = new o.Inner

    }
    object Extractor {
      def unapply(a: Any): Option[Outer] = Some(o1)
    }
    null match {
      case Extractor(o2) =>
        val i = new o2.Inner
    }
  }
}
