object Macros {
  def macro bar(x: Int): Int = Apply(Select(Select(Ident("scala"), newTermName("Predef")), newTermName("println")), List(Literal(Constant("object-Int"))))
  def macro bar(x: String): String = Apply(Select(Select(Ident("scala"), newTermName("Predef")), newTermName("println")), List(Literal(Constant("object-String"))))
}

class Macros {
  def macro bar(x: Int): Int = Apply(Select(Select(Ident("scala"), newTermName("Predef")), newTermName("println")), List(Literal(Constant("class-Int"))))
  def macro bar(x: String): String = Apply(Select(Select(Ident("scala"), newTermName("Predef")), newTermName("println")), List(Literal(Constant("class-String"))))
}