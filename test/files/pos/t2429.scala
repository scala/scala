object Msg {
  trait T

  trait TSeq

  object TSeq {
    implicit def fromSeq(s: Seq[T]): TSeq = error("stub")
  }

  def render {
    val msgs: TSeq = (List[(Any, Any)]().flatMap {
      case (a, b) => {
        a match {
          case _ => b match {
            case _ => error("stub")
          }
        }
      }
    } /*: Seq[T] Adding this type annotation avoids the compile error.*/)
  }
}
