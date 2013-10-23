import scala.language.experimental.macros

class C private () {
  def this(x: Int) = macro Macros.ctor
}