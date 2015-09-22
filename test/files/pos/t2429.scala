object Msg {
  trait T

  trait TSeq

  object TSeq {
    implicit def fromSeq(s: Seq[T]): TSeq = sys.error("stub")
  }

  def render {
    val msgs: TSeq = (List[(Any, Any)]().flatMap {
      case (a, b) => {
        a match {
          case _ => b match {
            case _ => sys.error("stub")
          }
        }
      }
    } /*: Seq[T] Adding this type annotation avoids the compile error.*/)
  }
}
object Oops {
 implicit def someImplicit(s: Seq[_]): String = sys.error("stub")
 def item: String = Nil map { case e: Any => e }
}
