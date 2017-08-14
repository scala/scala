object Test extends App {
  def callMacro[T: TC]: List[String] = implicitly[TC[T]].subclasses
  assert(callMacro[ADT] == List("class ADTConstructor"))
}

sealed trait ADT
object ADT {
  final case class ADTConstructor() extends ADT
}
