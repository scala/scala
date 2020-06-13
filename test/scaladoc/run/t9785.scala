import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {
  override def code = """
        /** @define macro Super */
        class Super {
          /** $macro */
          def inherited: Int = 5
          /** $macro */
          def implemented: Unit
          /** $macro */
          def overridden: String = "test"
        }

        /**
         * @define name default
         * @define dummy dummy
         */
        trait A[T] {
          /** List $name */
          def list(): List[T]
          /** Gets the $name with the given $dummy */
          def get(id: String): Option[T] = None
        }

        /** @define dummy id */
        trait B extends A[String]

        /**
         * @define macro Sub
         * @define name banana
         */
        class Sub extends Super with B {
          def list(): List[String] = List.empty
          def implemented: Unit = ()
          override def overridden: String = "overridden test"
        }
    """

  override def scaladocSettings: String = ""

  def assertCommentContains(method: Def, value: String): Unit = {
    val commentBody = method.comment.get.body.toString
    assert(commentBody.contains(value), s""""$commentBody".contains("$value")""")
  }

  def testModel(rootPackage: Package) = {
    import access._

    for {
      clasz <- List("Super", "Sub")
      method <- List("inherited", "implemented", "overridden")
    }{
      assertCommentContains(rootPackage._class(clasz)._method(method), clasz)
    }

    val sub = rootPackage._class("Sub")
    assertCommentContains(sub._method("list"), "List banana")
    assertCommentContains(sub._method("get"), "Gets the banana with the given id")
  }
}
