import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def code = """
  object Box {

    implicit def anyToBox[T](t: T): Box[T] = new Box(t)

  }

  class Box[T](val t: T)
  """

  def scaladocSettings = "-implicits"

  def testModel(root: Package) = {
    import access._

    // this used to contain the conversion to Box[Box[T]],
    // but not anymore.
    val conversions = root._class("Box").conversions
    println(conversions.map(_.targetType).mkString("\n"))
  }
}
