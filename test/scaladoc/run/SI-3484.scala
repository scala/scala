import scala.tools.nsc.doc.model._
import scala.tools.nsc.doc.model.diagram._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def code = """
        class cbf[A, B, C]

        /**
         *  @define Coll Collection
         *  @define bfreturn $Coll
         */
        class Collection[A] {
          /** What map does...
           *
           *  $bfreturn
           *  @usecase def map[B](f: A => B): $bfreturn[B]
           *
           */
          def map[B, That](f: A => B)(implicit fact: cbf[Collection[A], B, That]) =
            null
        }

        /**
         *  @define b John
         *  @define a Mister $b
         */
        class SR704 {
          /**
           *  Hello $a.
           */
          def foo = 123
        }
    """

  // diagrams must be started. In case there's an error with dot, it should not report anything
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // check correct expansion of the use case signature
    val map = rootPackage._class("Collection")._method("map")
    assert(map.resultType.name == "Collection[B]", map.resultType.name + " == Traversable[B]")

    val foo = rootPackage._class("SR704")._method("foo")
    assert(extractCommentText(foo.comment.get).contains("Hello Mister John."),
           extractCommentText(foo.comment.get) + ".contains(Hello Mister John.)")
  }
}