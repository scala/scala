import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
:setting -Xsource:2.12 -feature
case class C12(clazz: Class[_])
val o: Option[Class[T] forSome { type T}] = C12.unapply(C12(classOf[String]))

:setting -Xsource:2.11 -feature
import scala.language.existentials
case class C11(clazz: Class[_])
val o: Option[Class[T]] forSome { type T } = C11.unapply(C11(classOf[String]))
  """

  override def show() = {
    val r = eval().mkString("\n")
    println(r.linesIterator.filter(x => x.contains("warning") || x.contains("error")).mkString)
  }
}
