/** Test contributed by Stefan Zeiger showing that HLists can be
 *  specialized.
 */

sealed trait HList {
  type Self <: HList

  type |: [E] = HCons[E, Self]

  final def |: [@specialized E](elem: E): |: [E] = new HCons[E, Self](elem, this.asInstanceOf[Self])

  def m[@specialized E, T <: AnyRef](x: E): T = null.asInstanceOf[T]
}

final class HCons[@specialized H, T <: HList](val head: H, val tail: T) extends HList {
  type Self = HCons[H, T]
}

final object HNil extends HList {
  type Self = HNil.type
}

object Test extends App {
  val l1 = new HCons(42, "foo" |: HNil)
  println(l1.getClass)

  val l2 = 42 |: "abc" |: HNil
  println(l2.getClass)
}
