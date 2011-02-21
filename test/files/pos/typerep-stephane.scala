object typerep {

 class TypeRep[T] {
   def getType: TypeRep[T] = this
 }

 object BooleanRep extends TypeRep[Boolean] {
   override def toString = "Boolean"
 }
 object CharRep extends TypeRep[Char] {
   override def toString = "Char"
 }
 object IntRep  extends TypeRep[Int] {
   override def toString = "Int"
 }
 object LongRep extends TypeRep[Long] {
   override def toString = "Long"
 }
 object FloatRep extends TypeRep[Float] {
   override def toString = "Float"
 }
 object DoubleRep extends TypeRep[Double] {
   override def toString = "Double"
 }
 class ListRep[U, T <: List[U]](val elemRep: TypeRep[U]) extends TypeRep[T] {
   override def toString = "List[" + elemRep + "]"
 }

 implicit def typeRep(x: Boolean): TypeRep[Boolean] = BooleanRep
 implicit def typeRep(x: Char   ): TypeRep[Char   ] = CharRep
 implicit def typeRep(x: Long   ): TypeRep[Long   ] = LongRep
 implicit def typeRep(x: Float  ): TypeRep[Float  ] = FloatRep
 implicit def typeRep(x: Double ): TypeRep[Double ] = DoubleRep
 implicit def typeRep(x: Int    ): TypeRep[Int    ] = IntRep
/*
 implicit def typeRep[T](xs: List[T])(implicit rep: T => TypeRep[T]): TypeRep[List[T]] =
   new ListRep(rep(xs.head))
*/
 implicit def typeRep[T <% TypeRep[T]](xs: List[T]): TypeRep[List[T]] =
   new ListRep(xs.head)

}

object test extends App {
 import typerep._
 println(3.getType)
 println(List(3).getType)
}
