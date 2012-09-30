import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import language._

object Test extends ScaladocModelTest {

  // test a file instead of a piece of code
  override def resourceFile = "implicits-known-type-classes-res.scala"

  // start implicits
  def scaladocSettings = "-implicits"

  def testModel(root: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    /** Tests the "known type classes" feature of scaladoc implicits
     *  if the test fails, please update the correct qualified name of
     *  the type class in src/compiler/scala/tools/nsc/doc/Settings.scala
     *  in the knownTypeClasses map. Thank you! */

    val base = root._package("scala")._package("test")._package("scaladoc")._package("implicits")._package("typeclasses")
    var conv: ImplicitConversion = null

    val A = base._class("A")

    for (conversion <- A.conversions if !conversion.isHiddenConversion) {
      assert(conversion.constraints.length == 1, conversion.constraints.length + " == 1 (in " + conversion + ")")
      assert(conversion.constraints.head.isInstanceOf[KnownTypeClassConstraint],
             conversion.constraints.head + " is not a known type class constraint!")
    }
  }
}
