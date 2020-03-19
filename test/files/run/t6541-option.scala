import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
case class C(clazz: Class[_])
val o: Option[Class[T] forSome { type T}] = C.unapply(C(classOf[String]))
  """

  override def eval() = super.eval().filter(s => s.contains("warning") || s.contains("error"))
}
