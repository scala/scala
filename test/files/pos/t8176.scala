object Test {
  implicit lazy val ambiguousPostfixOps1: language.postfixOps.type = language.postfixOps
  implicit lazy val ambiguousPostfixOps2: language.postfixOps.type = language.postfixOps
  List(1, 2, 3) tail
}