import xml.Node

object Test extends App {
  val body: Node = <elem>hi</elem>	
  println ((body: AnyRef, "foo") match {
    case (node: Node, "bar")        => "bye"
    case (ser: Serializable, "foo") => "hi"
  })

  println ((body, "foo") match {
    case (node: Node, "bar")        => "bye"
    case (ser: Serializable, "foo") => "hi"
  })

  println ((body: AnyRef, "foo") match {
    case (node: Node, "foo")        => "bye"
    case (ser: Serializable, "foo") => "hi"
  })

  println ((body: AnyRef, "foo") match {
    case (node: Node, "foo")        => "bye"
    case (ser: Serializable, "foo") => "hi"
  })
}
// t4124.scala:7:
//     case (ser: Serializable, "foo") => "hi"
//                ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  AnyRef
//         pt  Object
//      pattp  Serializable
//   pattp+pt  Serializable
//   pt+pattp  Serializable
//     result  Serializable
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }// t4124.scala:12:
//     case (ser: Serializable, "foo") => "hi"
//                ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  scala.xml.Node
//         pt  scala.xml.Node
//      pattp  Serializable
//   pattp+pt  Serializable with scala.xml.Node
//   pt+pattp  scala.xml.Node with Serializable
//     result  scala.xml.Node with Serializable
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }// t4124.scala:17:
//     case (ser: Serializable, "foo") => "hi"
//                ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  AnyRef
//         pt  Object
//      pattp  Serializable
//   pattp+pt  Serializable
//   pt+pattp  Serializable
//     result  Serializable
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }// t4124.scala:22:
//     case (ser: Serializable, "foo") => "hi"
//                ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  AnyRef
//         pt  Object
//      pattp  Serializable
//   pattp+pt  Serializable
//   pt+pattp  Serializable
//     result  Serializable
//
//        pt0 =:= pt             pt0 !:= pattp     pattp+pt <:< pt0       pt+pattp <:< pt0         result <:< pt0
//         pt !:= pattp     pattp+pt <:< pt        pt+pattp <:< pt          result <:< pt
//   pattp+pt <:< pattp     pt+pattp <:< pattp       result <:< pattp
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }