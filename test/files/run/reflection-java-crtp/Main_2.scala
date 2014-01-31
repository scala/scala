object Test extends App {
  import scala.reflect.runtime.universe._
  val enum = typeOf[JavaSimpleEnumeration_1].baseClasses(1).asClass
  // make sure that the E's in Enum<E extends Enum<E>> are represented by the same symbol
  val e1 = enum.typeParams(0).asType
  val TypeBounds(_, TypeRef(_, _, List(TypeRef(_, e2: TypeSymbol, _)))) = e1.info
  println(e1, e2, e1 eq e2)
}