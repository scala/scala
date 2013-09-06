trait Y
trait Z extends Y
class X[+A <: Y]

object Test {
  def f1(x: X[_ <: Y]) = x match {
    case _: X[Any] => // looks a little funny; `Any` is outside the bounds for `A`
  }
  def f2(x: X[_ <: Y]) = x match {
    case _: X[Y]   => // looks better, let's allow this (too)
  }

  // NonLocalReturnControl[_] warnings
  def foo: Int = List(0).foldLeft(0){case _ => return 0}
}
// unchecked-a.scala:10:
//     case _: X[Y]   => // looks better, let's allow this (too)
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  X[_$2]
//         pt  X[_$2]
//      pattp  X[Y]
//   pattp+pt  X[Y]
//   pt+pattp  X[_$2]
//     result  X[Y]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }