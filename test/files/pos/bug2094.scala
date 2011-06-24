object Test extends App {
  // compiles:
  Map[Int, Value](
    0 -> KnownType(classOf[Object]),
    1 -> UnknownValue())

  // does not compile:
  Map(
    0 -> KnownType(classOf[Object]),
    1 -> UnknownValue())

  // Experiment.scala:10: error: type mismatch;
  //  found   : (Int, KnownType)
  //  required: (Int, Product with Value{def getType: Option[java.lang.Class[_$$2]]}) where type _$$2
  //     0 -> KnownType(classOf[Object]),
  //       ^
  // one error found
}
sealed trait Value {
  def getType: Option[Class[_]]
}

case class UnknownValue() extends Value {
  def getType = None
  // compiles if changed to:
  // def getType: Option[Class[_]] = None
}

case class KnownType(typ: Class[_]) extends Value {
  def getType = Some(typ)
}