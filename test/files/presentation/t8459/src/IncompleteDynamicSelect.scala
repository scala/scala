import scala.language.dynamics

class F extends Dynamic {
  def applyDynamic(name: String)(args: Any*) =
    s"method '$name' called with arguments ${args.mkString("'", "', '", "'")}"
}

class Foo {
  val bar = new F

  bar. //note whitespace after dot
  /*?*/ //force typechecking
}

