import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  // Working around the fact that usecases have the form Coll[T] and not Coll[T, U], as required by Map
  override def code = """
      /**
       * @define Coll C[T]
       */
      class C[T] {
        /**
         * @usecase def foo[T]: $Coll[T]
         */
        def foo[T: Numeric]: C[T]
      }


      /**
       * @define Coll D1[T]
       */
      class D[U, T] extends C[T] {
        protected type D1[Z] = D[U, Z]
      }
  """

  // no need for special settings
  def scaladocSettings = ""

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // just need to check the member exists, access methods will throw an error if there's a problem
    assert(rootPackage._class("D")._method("foo").resultType.name == "D[U, T]",
           rootPackage._class("D")._method("foo").resultType.name + " == D[U, T]")
  }
}