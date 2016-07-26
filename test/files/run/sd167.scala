object Test {
  implicit class ToExtractor(val s: StringContext) {
    def x = {println("x"); Some }
  }
  def main(args: Array[String]) {
    Some(1) match { case x"${a}" => }  // used to convert to `case Some(a) =>` and omit side effects
  }
}
