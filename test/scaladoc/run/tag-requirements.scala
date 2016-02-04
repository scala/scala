import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code =
    """
      package scala.test.scaladoc.tagrequirements
        /**
         * object comment
         * @version 1.0
         * @version 2.0
         */
        object Test {
          /**
           * foo comment
           * @param
           */
          def foo(b: Any) = ???
          /**
           * bar comment
           * @param b A value
           * @param b A value
           */
          def bar(b: Any) = ???
          /**
           * baz comment
           * @unrecognised
           */
          def baz() = ???
        }
    """

  def scaladocSettings = ""

  def testModel(root: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    val base = root._package("scala")._package("test")._package("scaladoc")._package("tagrequirements")

    val test = base._object("Test")
    /*
     * We only care about the warnings which are side effects but we assert on the comment to
     * avoid static code analysis noise about unused values.
     */
    assert(extractCommentText(test.comment.get) == "object comment")
    assert(extractCommentText(test._method("foo").comment.get) == "foo comment")
    assert(extractCommentText(test._method("bar").comment.get) == "bar comment")
    assert(extractCommentText(test._method("baz").comment.get) == "baz comment")
  }
}
