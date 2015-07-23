// this crazy code simply tries to nest pattern matches so that the last call is in a tricky-to-determine
// tail position (my initial tightening of tailpos detection for SI-6089 ruled this out)
class BKTree {
 @annotation.tailrec
 final def -?-[AA](a: AA): Boolean = this match {
    case BKTreeEmpty => false
    case BKTreeNode(v) => {
      val d = 1
      d == 0 || ( Map(1 -> this,2  -> this,3 -> this) get d match {
        case None => false
        case Some(w) => w -?- a // can tail call here (since || is shortcutting)
      })
    }
  }
}

object BKTreeEmpty extends BKTree
case class BKTreeNode[A](v: A) extends BKTree