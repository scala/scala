import scala.tools.nsc._

trait Foo {
  val global: Global
  import global.{Name, Symbol, nme}

  case class Bippy(name: Name)

  def f(x: Bippy, sym: Symbol): Int = {
    // no warning (!) for
    // val Stable = sym.name.toTermName

    val Stable = sym.name
    Bippy(Stable) match {
      case Bippy(nme.WILDCARD) => 1
      case Bippy(Stable) => 2 // should not be considered unreachable
      case Bippy(_) => 3
    }
  }
}