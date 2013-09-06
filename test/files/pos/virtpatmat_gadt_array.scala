import scala.collection.mutable._
object Test {
  def genericArrayOps[T](xs: Array[T]): ArrayOps[T] = xs match {
    case x: Array[AnyRef]  => refArrayOps[AnyRef](x).asInstanceOf[ArrayOps[T]]
    case null              => null
  }
  // def genericArrayOps[T >: Nothing <: Any](xs: Array[T]): scala.collection.mutable.ArrayOps[T] 
  //   = OptionMatching.runOrElse(xs)(((x1: Array[T]) => 
  //     ((OptionMatching.guard(x1.isInstanceOf[Array[AnyRef]], x1.asInstanceOf[Array[T] with Array[AnyRef]]).flatMap(((x2: Array[T] with Array[AnyRef]) => 
  //       OptionMatching.one(Test.this.refArrayOps[AnyRef](x2).asInstanceOf[scala.collection.mutable.ArrayOps[T]]))): Option[scala.collection.mutable.ArrayOps[T]]).orElse(
  //     (OptionMatching.guard(null.==(x1), x1.asInstanceOf[Array[T]]).flatMap(((x3: Array[T]) => 
  //         OptionMatching.one(null))): Option[scala.collection.mutable.ArrayOps[T]])): Option[scala.collection.mutable.ArrayOps[T]]).orElse((OptionMatching.zero: Option[scala.collection.mutable.ArrayOps[T]]))))
  
  def refArrayOps[T <: AnyRef](xs: Array[T]): ArrayOps[T] = new ArrayOps.ofRef[T](xs)
}// virtpatmat_gadt_array.scala:4:
//     case x: Array[AnyRef]  => refArrayOps[AnyRef](x).asInstanceOf[ArrayOps[T]]
//             ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  Array[T]
//         pt  Array[T]
//      pattp  Array[AnyRef]
//   pattp+pt  Array[AnyRef]
//   pt+pattp  Array[T]
//     result  Array[AnyRef]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 ~:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt ~:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp =:= result
//   pattp+pt ~:= pt+pattp  pattp+pt =:= result
//   pt+pattp ~:= result
//
// }