package tastytest

class Reflection { reflected =>

  private val FIELD_1 = 23
  private val FIELD_2 = "hello"

  @throws(classOf[IndexOutOfBoundsException])
  def fieldAtIndex(n: Int): Any =
    try {
      getClass.getFields()(n).get(reflected)
    } catch {
      case ex: ArrayIndexOutOfBoundsException =>
        throw new IndexOutOfBoundsException(n.toString())
    }
}
