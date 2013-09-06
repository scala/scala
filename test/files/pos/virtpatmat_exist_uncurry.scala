object Test {
  trait Leaf[T] {
    def collect[U](f: PartialFunction[Leaf[_], U]): List[U]
    def leaves: List[Leaf[T]] = collect { case l: Leaf[T] => l }
  }
}// virtpatmat_exist_uncurry.scala:4:
//     def leaves: List[Leaf[T]] = collect { case l: Leaf[T] => l }
//                                                   ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Test.Leaf[_$1]
//         pt  Test.Leaf[_$1]
//      pattp  Test.Leaf[T]
//   pattp+pt  Test.Leaf[T]
//   pt+pattp  Test.Leaf[_$1]
//     result  Test.Leaf[T]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }// virtpatmat_exist_uncurry.scala:4:
//     def leaves: List[Leaf[T]] = collect { case l: Leaf[T] => l }
//                                                   ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Test.Leaf[_$1]
//         pt  Test.Leaf[_$1]
//      pattp  Test.Leaf[T]
//   pattp+pt  Test.Leaf[T]
//   pt+pattp  Test.Leaf[_$1]
//     result  Test.Leaf[T]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }