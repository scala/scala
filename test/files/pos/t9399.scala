sealed abstract class TA
sealed abstract class TB extends TA
case object A extends TA
case object B extends TB

sealed trait C
case class CTA(id: Int, da: TA) extends C
case class CTB(id: Int, da: TB) extends C

class Test {
  def test(c: C): Unit = c match {
    case CTA(_, A) =>
    case CTA(_, B) =>
    case CTB(_, B) =>
  }
}

