class meh extends StaticAnnotation

class ALike[C]
abstract class AFactory[CC[x] <: ALike[CC[x]]] {
  def aCompanion : AFactory[CC @meh]
}