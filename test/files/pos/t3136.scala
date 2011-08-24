class Type
class Symbol
case class PolyType(tps: List[Symbol], res: Type) extends Type
class OtherType extends Type

// case class NullaryMethodType(tp: Type) extends Type

object NullaryMethodType {
  def apply(resTpe: Type): Type = PolyType(List(), resTpe)
  def unapply(tp: Type): Option[(Type)] = None
}

object Test {
  def TEST(tp: Type): String =
    tp match {
      case PolyType(ps1, PolyType(ps2, res @ PolyType(a, b))) => "1"+tp // couldn't find a simpler version that still crashes
      case NullaryMethodType(meh) => "2"+meh
    }
}
