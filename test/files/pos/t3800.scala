class meh extends annotation.StaticAnnotation

class ALike[C]
abstract class AFactory[CC[x] <: ALike[CC[x]]] {
  def aCompanion : AFactory[CC @meh]
}