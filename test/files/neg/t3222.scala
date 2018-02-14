@throws(classOf[B])
class ExceptionTest {
  @throws(classOf[C])
  def foo(@throws(classOf[D]) x: Int): Unit = {}

  @throws(classOf[E])
  type t = String
}

