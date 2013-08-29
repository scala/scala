object Test extends App {
  trait T

  trait TA
  trait TB

  class A extends T with TA
  class B extends T with TB
  class AB extends T with TA with TB
  // Matching on _: TA with TB

  val li: Vector[T] = Vector(new A, new B, new AB)

  val matched = (for (l <- li) yield {
    l match {
      case _: TA with TB => "tab"
      case _: TA => "ta"
      case _: TB => "tb"
    }
  })

  println(matched)
}// t5688.scala:16:
//       case _: TA with TB => "tab"
//               ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Test.T
//         pt  Test.T
//      pattp  Test.TA with Test.TB
//   pattp+pt  Test.TA with Test.TB with Test.T
//   pt+pattp  Test.T with Test.TA with Test.TB
//     result  Test.T with Test.TA with Test.TB
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }// t5688.scala:17:
//       case _: TA => "ta"
//               ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Test.T
//         pt  Test.T
//      pattp  Test.TA
//   pattp+pt  Test.TA with Test.T
//   pt+pattp  Test.T with Test.TA
//     result  Test.T with Test.TA
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }// t5688.scala:18:
//       case _: TB => "tb"
//               ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Test.T
//         pt  Test.T
//      pattp  Test.TB
//   pattp+pt  Test.TB with Test.T
//   pt+pattp  Test.T with Test.TB
//     result  Test.T with Test.TB
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }