import scala.tools.partest._

object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -Vimplicits -Vimplicits-verbose-tree"

  def code: String = ""

  def verboseTree: String = """
object tpes
{
  trait I1
  trait I2
  trait I3
  trait I4
  trait I5
  trait I6
  trait I7
  trait I8
  trait I9
}
import tpes._

object Tree
{
  implicit def i8(implicit p: I9): I8 = ???
  implicit def i7(implicit p: I8): I7 = ???
  implicit def i6a(implicit p: I7): I6 = ???
  implicit def i6b(implicit p: I8): I6 = ???
  implicit def i5(implicit p: I6): I5 = ???
  implicit def i4(implicit p: I5): I4 = ???
  implicit def i3a(implicit p: I4): I3 = ???
  implicit def i3b(implicit p: I4): I3 = ???
  implicit def i2(implicit p: I3): I2 = ???
  implicit def i1a(implicit p: I2): I1 = ???
  implicit def i1b(implicit p: I6): I1 = ???
  implicitly[I1]
}
  """

  def show(): Unit = {
    val global = newCompiler()

    def run(code: String): Unit =
      compileString(global)(code.trim)

    run(verboseTree)
  }
}
