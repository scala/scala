//> using options -Werror -Xlint:strict-unsealed-patmat
//
// More than 16 equality symbols per variable (so implications are looked up by base class): mixins, a sealed
// sub-hierarchy, abstract and singleton type patterns, value classes, existentials, and tuples of such.
sealed trait T
sealed trait S extends T
trait M1; trait M2
final class VC(val i: Int) extends AnyVal
final case class C0(a: Int) extends T with M1
final case class C1(a: Int) extends T with M2
final case class C2(a: Int) extends T with M1 with M2
final case class C3(a: Int) extends S
final case class C4(a: Int) extends S with M1
case object C5 extends S
case object C6 extends T with M2
final case class C7(a: Int) extends T
final case class C8(a: Int) extends T
final case class C9(a: Int) extends T
final case class C10(a: Int) extends T
final case class C11(a: Int) extends T
final case class C12(a: Int) extends T
final case class C13(a: Int) extends T
final case class C14(a: Int) extends T
final case class C15(a: Int) extends T
final case class C16(a: Int) extends T
final case class C17(a: Int) extends T
final case class C18(a: Int) extends T
object Test {
  def f(t: T): Int = t match {
    case _: M1 => 1
    case _: (M1 with M2) => 2
    case _: S => 3
    case C5 => 4
    case _: C6.type => 5
    case C3(1) => 6
    case C7(_) => 7
    case null => 8
    case _: T => 9
    case _: Any => 10
    case _: Serializable => 11
    case _: Product => 12
    case _: M2 => 13
  }
  def g[X <: S](x: X, t: T): Int = t match {
    case _: X => 1
    case _: x.type => 2
    case _: S => 3
    case _: M1 => 4
    case C0(_) => 5
    case C1(_) => 6
    case C2(_) => 7
    case C3(_) => 8
    case C4(_) => 9
    case C5 => 10
    case C6 => 11
    case C7(_) => 12
    case C8(_) => 13
    case C9(_) => 14
    case C10(_) => 15
    case C11(_) => 16
    case C12(_) => 17
    case _ => 18
  }
  def h(a: Any): Int = a match {
    case _: VC => 1
    case _: Int => 2
    case _: List[_] => 4
    case _: List[Int] => 5
    case _: Seq[_] => 6
    case _: Iterable[_] => 7
    case _: Array[Int] => 8
    case _: Array[_] => 9
    case _: Function1[_, _] => 10
    case _: (Int => Int) => 11
    case _: Unit => 14
    case _: String => 15
    case _: CharSequence => 16
    case _: Comparable[_] => 17
    case _: Product => 18
    case _ => 19
  }
  def ex(t: T, o: Option[T]): Int = (t, o) match {
    case (C0(_), Some(C0(_))) => 1
    case (C1(_), Some(C1(_))) => 2
    case (C2(_), Some(C2(_))) => 3
    case (C3(_), Some(C3(_))) => 4
    case (C4(_), Some(C4(_))) => 5
    case (C5, Some(C5)) => 6
    case (C6, Some(C6)) => 7
    case (C7(_), Some(C7(_))) => 8
    case (C8(_), Some(C8(_))) => 9
    case (C9(_), Some(C9(_))) => 10
    case (C10(_), Some(C10(_))) => 11
    case (C11(_), Some(C11(_))) => 12
    case (C12(_), Some(C12(_))) => 13
    case (C13(_), Some(C13(_))) => 14
    case (C14(_), Some(C14(_))) => 15
    case (C15(_), Some(C15(_))) => 16
    case (C16(_), Some(C16(_))) => 17
    case (C17(_), Some(C17(_))) => 18
    case (C18(_), None) => 19
  }
}
