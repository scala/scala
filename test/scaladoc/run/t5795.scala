import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
/**
  * Only the 'deprecated' tag should stay.
  *
  * @author
  * @since
  * @todo
  * @note
  * @see
  * @version
  * @deprecated
  * @example
  * @constructor
  */
object Test {
  /**
    * Only the 'throws' tag should stay.
    * @param foo
    * @param bar
    * @param baz
    * @return
    * @throws Exception
    * @tparam T
    */
  def foo[T](foo: Any, bar: Any, baz: Any): Int = 1
}
  """

  def scaladocSettings = ""

  def test(b: Boolean, text: => String): Unit = if (!b) println(text)

  def testModel(root: Package) = {
    import access._
    val obj = root._object("Test")
    val c = obj.comment.get

    test(c.authors.isEmpty, s"expected no authors, found: ${c.authors}")
    test(!c.since.isDefined, s"expected no since tag, found: ${c.since}")
    test(c.todo.isEmpty, s"expected no todos, found: ${c.todo}")
    test(c.note.isEmpty, s"expected no note, found: ${c.note}")
    test(c.see.isEmpty, s"expected no see, found: ${c.see}")
    test(!c.version.isDefined, s"expected no version tag, found: ${c.version}")
    // deprecated stays
    test(c.deprecated.isDefined, s"expected deprecated tag, found none")
    test(c.example.isEmpty, s"expected no example, found: ${c.example}")
    test(!c.constructor.isDefined, s"expected no constructor tag, found: ${c.constructor}")

    val method = obj._method("foo")
    val mc = method.comment.get

    test(mc.valueParams.isEmpty, s"expected empty value params, found: ${mc.valueParams}")
    test(mc.typeParams.isEmpty, s"expected empty type params, found: ${mc.typeParams}")
    test(!mc.result.isDefined, s"expected no result tag, found: ${mc.result}")
    // throws stay
    test(!mc.throws.isEmpty, s"expected an exception tag, found: ${mc.throws}")
  }
}
