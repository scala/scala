package tester

object PatMatWarning {

  sealed trait X
  sealed trait Y

  def f(x: X) = x match {
    case _: Y => false
    case _    => true
  }

  class X1 extends X
  class Y1 extends Y
  class Z1 extends X with Y
}
// t6537.scala:9:
//     case _: Y => false
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  tester.PatMatWarning.X
//         pt  tester.PatMatWarning.X
//      pattp  tester.PatMatWarning.Y
//   pattp+pt  tester.PatMatWarning.Y with tester.PatMatWarning.X
//   pt+pattp  tester.PatMatWarning.X with tester.PatMatWarning.Y
//     result  tester.PatMatWarning.X with tester.PatMatWarning.Y
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }