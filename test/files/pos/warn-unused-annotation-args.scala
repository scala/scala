//> using options -Werror -Wunused:privates

class ann(s: String) extends scala.annotation.ConstantAnnotation
class K

// Each `Priv` is referenced only by an annotation argument. The annotations
// are equal, but their arguments come from different trees.
class InType1 {
  private final val Priv = "x"
  def f: K @ann(s = Priv) = null
}
class InType2 {
  private final val Priv = "x"
  def f: K @ann(s = Priv) = null
}
class OnDef1 {
  private final val Priv = "x"
  @ann(s = Priv) def f = 0
}
class OnDef2 {
  private final val Priv = "x"
  @ann(s = Priv) def f = 0
}

class InClassOf {
  private final val Priv = "y"
  final val c = classOf[K @ann(s = Priv)]
}
