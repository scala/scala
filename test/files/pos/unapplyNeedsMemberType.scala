// error with -Xunapply, (because of missing call to memberType?)

trait Gunk[a] {

  type Seq

  object Cons {
    def unapply(s: Seq) = unapply_Cons(s)
  }
  def unapply_Cons(s: Any): Option[Tuple2[a, Seq]]
}  

class Join[a] extends Gunk[a] {
  type Seq = JoinSeq

  abstract class JoinSeq
  case class App(xs: Seq, ys: Seq) extends JoinSeq

  def append(s1: Seq, s2: Seq): Seq = s1 // mock implementation

  def unapply_Cons(s: Any) = s match {
    case App(Cons(x, xs), ys) => Some(Pair(x, append(xs, ys)))
    case _ => null
  }
}
