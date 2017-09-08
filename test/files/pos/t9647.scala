sealed trait HList
case class HCons[H, T <: HList](head: H, tail: T) extends HList
case object HNil extends HList

object Test {

  @annotation.tailrec
  def foo[L <: HList](l: L): Unit = l match {
    case HNil => ()
    case HCons(h, t) => foo(t)
  }

}
