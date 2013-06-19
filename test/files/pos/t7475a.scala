trait AbstractPublic {
  def queue: Any
}
trait ConcretePrivate {
  private val queue: Any = ()
}

abstract class Mix
  extends ConcretePrivate with AbstractPublic {
  final def queue: Any = ()
}
