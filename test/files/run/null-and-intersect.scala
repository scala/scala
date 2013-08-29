object Test {
  trait Immortal
  class Bippy extends Immutable with Immortal
  class Boppy extends Immutable
  
  def f[T](x: Traversable[T]) = x match {
    case _: Map[_, _]   => 3
    case _: Seq[_]      => 2
    case _: Iterable[_] => 1
    case _              => 4
  }
  def g(x: Bippy) = x match {
    case _: Immutable with Immortal => 1
    case _                          => 2
  }
  def h(x: Immutable) = x match {
    case _: Immortal => 1
    case _           => 2
  }

  def main(args: Array[String]): Unit = {
    println(f(Set(1)))
    println(f(Seq(1)))
    println(f(Map(1 -> 2)))
    println(f(null))
    
    println(g(new Bippy))
    println(g(null))
    
    println(h(new Bippy))
    println(h(new Boppy))
    println(h(null))
  }
}
// null-and-intersect.scala:17:
//     case _: Immortal => 1
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Immutable
//         pt  Immutable
//      pattp  Test.Immortal
//   pattp+pt  Test.Immortal with Immutable
//   pt+pattp  Immutable with Test.Immortal
//     result  Immutable with Test.Immortal
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }