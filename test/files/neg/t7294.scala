object Test {
  // Treat TupleN as final under -Xfuture for the for the purposes
  // of the "fruitless type test" warning.
  (1, 2) match { case Seq() => 0; case _ => 1 }
}
// t7294.scala:4:
//   (1, 2) match { case Seq() => 0; case _ => 1 }
//                          ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  (Int, Int)
//         pt  (Int, Int)
//      pattp  Seq[A]
//   pattp+pt  Seq[A] with (Int, Int)
//   pt+pattp  (Int, Int) with Seq[A]
//     result  (Int, Int) with Seq[A]
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }