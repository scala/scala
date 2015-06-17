import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest
import language._

object Test extends ScaladocModelTest {

  // test a file instead of a piece of code
  override def resourceFile = "implicits-base-res.scala"

  // start implicits
  def scaladocSettings = "-implicits -implicits-show-all"

  def testModel(root: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    def isShadowed(mbr: MemberEntity): Boolean =
      mbr.byConversion.map(_.source.implicitsShadowing.get(mbr).map(_.isShadowed).getOrElse(false)).getOrElse(false)

    // SEE THE test/resources/implicits-base-res.scala FOR THE EXPLANATION OF WHAT'S CHECKED HERE:
    val base = root._package("scala")._package("test")._package("scaladoc")._package("implicits")._package("base")
    var conv: ImplicitConversion = null

//// class A ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val A = base._class("A")

    // def convToEnrichedA(x: T)                // enrichA0: with no constraints, SHADOWED
    conv = A._conversion(A.qualifiedName + ".enrichA0")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)
    assert(isShadowed(conv._member("convToEnrichedA")))
    assert(conv._member("convToEnrichedA").resultType.name == "T")

    // def convToNumericA: T               // enrichA1: with a constraint that there is x: Numeric[T] implicit in scope
    conv = A._conversion(A.qualifiedName + ".enrichA1")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 1)
    assert(conv._member("convToNumericA").resultType.name == "T")

    // def convToIntA: Int                 // enrichA2: with a constraint that T = Int
    conv = A._conversion(A.qualifiedName + ".enrichA2")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 1)
    assert(conv._member("convToIntA").resultType.name == "Int")

    // def convToGtColonDoubleA: Double    // enrichA3: with a constraint that T <: Double
    conv = A._conversion(A.qualifiedName + ".enrichA3")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 1)
    assert(conv._member("convToGtColonDoubleA").resultType.name == "Double")

    // def convToEnrichedA: S                // enrichA4: with 3 constraints: T = Foo[Bar[S]], S: Foo and S: Bar
    conv = A._conversion(A.qualifiedName + ".enrichA4")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 3)
    assert(conv._member("convToEnrichedA").resultType.name == "S")

    // def convToEnrichedA: Bar[Foo[T]]      // enrichA5: no constraints
    conv = A._conversion(A.qualifiedName + ".enrichA5")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)
    assert(isShadowed(conv._member("convToEnrichedA")))
    assert(conv._member("convToEnrichedA").resultType.name == "Bar[Foo[T]]")

    // def convToMyNumericA: T             // enrichA6: with a constraint that there is x: MyNumeric[T] implicit in scope
    conv = A._conversion(A.qualifiedName + ".enrichA6")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 1)
    assert(conv._member("convToMyNumericA").resultType.name == "T")

    // def convToManifestA: T              // enrichA7: with 2 constraints: T: Manifest and T <: Double
    // def convToTraversableOps: T         // enrichA7: with 2 constraints: T: Manifest and T <: Double
                                           // should not be abstract!
    conv = A._conversion(A.qualifiedName + ".enrichA7")
    assert(conv.members.length == 2)
    assert(conv.constraints.length == 2)
    assert(conv._member("convToManifestA").resultType.name == "T")
    assert(conv._member("convToTraversableOps").resultType.name == "T")
    assert(conv._member("convToTraversableOps").flags.toString.indexOf("abstract") == -1)

//// class B ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val B = base._class("B")

    // these conversions should not affect B
    assert(B._conversions(A.qualifiedName + ".enrichA2").isEmpty)
    assert(B._conversions(A.qualifiedName + ".enrichA4").isEmpty)

    // def convToEnrichedA(x: Double)           // enrichA0: no constraints, SHADOWED
    conv = B._conversion(A.qualifiedName + ".enrichA0")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)
    assert(isShadowed(conv._member("convToEnrichedA")))
    assert(conv._member("convToEnrichedA").resultType.name == "Double")

    // def convToNumericA: Double          // enrichA1: no constraints
    conv = B._conversion(A.qualifiedName + ".enrichA1")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)
    assert(conv._member("convToNumericA").resultType.name == "Double")

    // def convToGtColonDoubleA: Double    // enrichA3: no constraints
    conv = B._conversion(A.qualifiedName + ".enrichA3")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)
    assert(conv._member("convToGtColonDoubleA").resultType.name == "Double")

    // def convToEnrichedA: Bar[Foo[Double]] // enrichA5: no constraints
    conv = B._conversion(A.qualifiedName + ".enrichA5")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)
    assert(isShadowed(conv._member("convToEnrichedA")))
    assert(conv._member("convToEnrichedA").resultType.name == "Bar[Foo[Double]]")

    // def convToMyNumericA: Double        // enrichA6: (if showAll is set) with a constraint that there is x: MyNumeric[Double] implicit in scope
    conv = B._conversion(A.qualifiedName + ".enrichA6")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 1)
    assert(conv._member("convToMyNumericA").resultType.name == "Double")

    // def convToManifestA: Double         // enrichA7: no constraints
    // def convToTraversableOps: Double    // enrichA7: no constraints
    //                                     // should not be abstract!
    conv = B._conversion(A.qualifiedName + ".enrichA7")
    assert(conv.members.length == 2)
    assert(conv.constraints.length == 0)
    assert(conv._member("convToManifestA").resultType.name == "Double")
    assert(conv._member("convToTraversableOps").resultType.name == "Double")
    assert(conv._member("convToTraversableOps").flags.toString.indexOf("abstract") == -1)

//// class C ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val C = base._class("C")

    // these conversions should not affect C
    assert(C._conversions(A.qualifiedName + ".enrichA3").isEmpty)
    assert(C._conversions(A.qualifiedName + ".enrichA4").isEmpty)
    assert(C._conversions(A.qualifiedName + ".enrichA7").isEmpty)

    // def convToEnrichedA(x: Int)           // enrichA0: no constraints, SHADOWED
    conv = C._conversion(A.qualifiedName + ".enrichA0")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)
    assert(isShadowed(conv._member("convToEnrichedA")))
    assert(conv._member("convToEnrichedA").resultType.name == "Int")

    // def convToNumericA: Int             // enrichA1: no constraints
    conv = C._conversion(A.qualifiedName + ".enrichA1")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)
    assert(conv._member("convToNumericA").resultType.name == "Int")

    // def convToIntA: Int                 // enrichA2: no constraints
    conv = C._conversion(A.qualifiedName + ".enrichA2")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)
    assert(conv._member("convToIntA").resultType.name == "Int")

    // def convToEnrichedA: Bar[Foo[Int]]    // enrichA5: no constraints
    conv = C._conversion(A.qualifiedName + ".enrichA5")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)
    assert(isShadowed(conv._member("convToEnrichedA")))
    assert(conv._member("convToEnrichedA").resultType.name == "Bar[Foo[Int]]")

    // def convToMyNumericA: Int           // enrichA6: (if showAll is set) with a constraint that there is x: MyNumeric[Int] implicit in scope
    conv = C._conversion(A.qualifiedName + ".enrichA6")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 1)
    assert(conv._member("convToMyNumericA").resultType.name == "Int")

//// class D ///////////////////////////////////////////////////////////////////////////////////////////////////////////

    val D = base._class("D")

    // these conversions should not affect D
    assert(D._conversions(A.qualifiedName + ".enrichA2").isEmpty)
    assert(D._conversions(A.qualifiedName + ".enrichA3").isEmpty)
    assert(D._conversions(A.qualifiedName + ".enrichA4").isEmpty)
    assert(D._conversions(A.qualifiedName + ".enrichA7").isEmpty)

    // def convToEnrichedA(x: String)        // enrichA0: no constraints, SHADOWED
    conv = D._conversion(A.qualifiedName + ".enrichA0")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)
    assert(isShadowed(conv._member("convToEnrichedA")))
    assert(conv._member("convToEnrichedA").resultType.name == "String")

    // def convToNumericA: String          // enrichA1: (if showAll is set) with a constraint that there is x: Numeric[String] implicit in scope
    conv = D._conversion(A.qualifiedName + ".enrichA1")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 1)
    assert(conv._member("convToNumericA").resultType.name == "String")

    // def convToEnrichedA: Bar[Foo[String]] // enrichA5: no constraints
    conv = D._conversion(A.qualifiedName + ".enrichA5")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 0)
    assert(isShadowed(conv._member("convToEnrichedA")))
    assert(conv._member("convToEnrichedA").resultType.name == "Bar[Foo[String]]")

    // def convToMyNumericA: String        // enrichA6: (if showAll is set) with a constraint that there is x: MyNumeric[String] implicit in scope
    conv = D._conversion(A.qualifiedName + ".enrichA6")
    assert(conv.members.length == 1)
    assert(conv.constraints.length == 1)
    assert(conv._member("convToMyNumericA").resultType.name == "String")
  }
}
