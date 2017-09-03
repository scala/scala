object Test extends App {
  def callMacro[T: TC]: List[String] = implicitly[TC[T]].subclasses
  assert(callMacro[ADT] == List("class ADTConstructor"))
  assert(ADT.instance.subclasses == List("class ADTConstructor"))
  assert(ADT.ADTConstructor.ctorInstance.subclasses == Nil)
}

sealed trait ADT
object ADT {
  final val instance = implicitly[TC[ADT]]

  final case class ADTConstructor() extends ADT
  object ADTConstructor {
    final val ctorInstance = implicitly[TC[ADTConstructor]]
  }
}
