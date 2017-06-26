case class CASEntry()
class CASCache extends Cache_1[CASEntry] {
  override def get(keys: AnyRef*): CASEntry = {
    super.get(keys: _*) // generates a direct `.super[Cache_1]` call, works
    foo(super.get(keys: _*)) // generates a superaccessor call, fails
  }

  def foo[T](f: => T): T = f
}

object Test extends App {
  new CASCache().get("")
}
