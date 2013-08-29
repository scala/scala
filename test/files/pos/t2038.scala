class Test {
  List(Some(classOf[java.lang.Integer]), Some(classOf[Int])).map {
    case Some(f: Class[_]) => f.cast(???)
  }
}// t2038.scala:3:
//     case Some(f: Class[_]) => f.cast(???)
//                  ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Class[_ >: Int with Integer]
//         pt  Class[_ >: Int with Integer]
//      pattp  Class[_]
//   pattp+pt  Class[_] with Class[_ >: Int with Integer]
//   pt+pattp  Class[_ >: Int with Integer] with Class[_]
//     result  Class[_ >: Int with Integer] with Class[_]
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }