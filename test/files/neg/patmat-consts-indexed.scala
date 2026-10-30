//> using options -Werror -Xlint:strict-unsealed-patmat
//
// More than 16 constants per match (`Const.IndexAbove`), of kinds that are compared for `=:=` across the index:
// class type refs under different (stable or alias) prefixes, abstract type members, Any/AnyRef/Object, singleton,
// literal and raw Java types.
class Outer { class In; object Mod }
object o1 extends Outer
object o2 extends Outer
trait Holder { type Elem }
trait IntHolder extends Holder { type Elem = Int }
object Test {
  type Alias1 = o1.In
  type Alias2 = Alias1
  val x1: Outer = o1
  def f(a: Any, h: IntHolder, o: Outer): Int = a match {
    case _: o1.In => 1
    case _: Alias1 => 2
    case _: Alias2 => 3
    case _: o2.In => 4
    case _: o.In => 5
    case _: x1.In => 6
    case _: h.Elem => 7
    case _: Int => 8
    case _: java.lang.Integer => 9
    case _: java.util.List[_] => 10
    case _: java.util.List[String] => 11
    case _: java.util.ArrayList[_] => 12
    case _: AnyRef => 13
    case _: Any => 14
    case _: Object => 15
    case _: String => 16
    case "x" => 17
    case 1 => 18
    case 1L => 19
    case o1.Mod => 20
    case o2.Mod => 21
    case null => 22
    case _: Singleton => 23
    case _: Product with Serializable => 24
    case _: (Product with Serializable) => 25
    case _: Unit => 27
    case () => 28
    case true => 29
    case _: Boolean => 30
    case _: Outer#In => 31
    case _: o1.type => 32
    case _ => 33
  }
}
