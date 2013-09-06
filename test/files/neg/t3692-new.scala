import scala.reflect.{ClassTag, classTag}
import java.lang.Integer

object Tester {
  def main(args: Array[String]) = {
    val map = Map("John" -> 1, "Josh" -> 2)
    new Tester().toJavaMap(map)
  }
}

class Tester {
  private final def toJavaMap[T, V](map: Map[T, V])(implicit m1: ClassTag[T], m2: ClassTag[V]): java.util.Map[_, _] = {
    map match {
      case m0: Map[Int, Int] => new java.util.HashMap[Integer, Integer]
      case m1: Map[Int, V] => new java.util.HashMap[Integer, V]
      case m2: Map[T, Int] => new java.util.HashMap[T, Integer]
      case _ => new java.util.HashMap[T, V]
    }
  }
}// t3692-new.scala:15:
//       case m1: Map[Int, V] => new java.util.HashMap[Integer, V]
//                ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Map[T,V]
//         pt  scala.collection.immutable.Map[T,V]
//      pattp  Map[Int,V]
//   pattp+pt  Map[Int,V]
//   pt+pattp  scala.collection.immutable.Map[T,V]
//     result  scala.collection.immutable.Map[T,V]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 =:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt =:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp ~:= result
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }