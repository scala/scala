object Test extends App {
  def callMacro[T: TC]: Unit = ()
  callMacro[ADT]
}

sealed trait ADT
object ADT {
  final case class ADTConstructor() extends ADT
}
