import scala.language.{ higherKinds, postfixOps }

@deprecated("Suppress warnings", since="2.11")
object Test
{
  object Variances extends Enumeration {
    val CO, IN, CONTRA = Value
  }
  import Variances.{ CO, IN, CONTRA }

  object SubtypeRelationship extends Enumeration {
    val NONE, SAME, SUB, SUPER = Value
  }
  import SubtypeRelationship.{ NONE, SAME, SUB, SUPER }

  class VarianceTester[T, U, CC[_]](expected: Variances.Value)(
            implicit ev1: Manifest[T], ev2: Manifest[U], ev3: Manifest[CC[T]], ev4: Manifest[CC[U]]) {

    def elements = List(ev1 <:< ev2, ev2 <:< ev1)
    def containers = List(ev3 <:< ev4, ev4 <:< ev3)

    def isUnrelated = typeCompare[T, U] == NONE
    def isSame = typeCompare[T, U] == SAME
    def isSub = typeCompare[T, U] == SUB
    def isSuper = typeCompare[T, U] == SUPER

    def showsCovariance = (elements == containers)
    def showsContravariance = (elements == containers.reverse)
    def showsInvariance = containers forall (_ == isSame)

    def allContainerVariances = List(showsCovariance, showsInvariance, showsContravariance)

    def showsExpectedVariance =
      if (isUnrelated) allContainerVariances forall (_ == false)
      else if (isSame) allContainerVariances forall (_ == true)
      else expected match {
        case CO     => showsCovariance && !showsContravariance && !showsInvariance
        case IN     => showsInvariance && !showsCovariance && !showsContravariance
        case CONTRA => showsContravariance && !showsCovariance && !showsInvariance
      }
  }

  def showsCovariance[T, U, CC[_]](implicit ev1: Manifest[T], ev2: Manifest[U], ev3: Manifest[CC[T]], ev4: Manifest[CC[U]]) =
    new VarianceTester[T, U, CC](CO) showsExpectedVariance

  def showsInvariance[T, U, CC[_]](implicit ev1: Manifest[T], ev2: Manifest[U], ev3: Manifest[CC[T]], ev4: Manifest[CC[U]]) =
    new VarianceTester[T, U, CC](IN) showsExpectedVariance

  def showsContravariance[T, U, CC[_]](implicit ev1: Manifest[T], ev2: Manifest[U], ev3: Manifest[CC[T]], ev4: Manifest[CC[U]]) =
    new VarianceTester[T, U, CC](CONTRA) showsExpectedVariance

  def typeCompare[T, U](implicit ev1: Manifest[T], ev2: Manifest[U]) = (ev1 <:< ev2, ev2 <:< ev1) match {
    case (true, true)   => SAME
    case (true, false)  => SUB
    case (false, true)  => SUPER
    case (false, false) => NONE
  }

  def assertAnyRef[T: Manifest] = List(
    manifest[T] <:< manifest[Any],
    manifest[T] <:< manifest[AnyRef],
    !(manifest[T] <:< manifest[AnyVal])
  ) foreach (assert(_, "assertAnyRef"))

  def assertAnyVal[T: Manifest] = List(
    manifest[T] <:< manifest[Any],
    !(manifest[T] <:< manifest[AnyRef]),
    manifest[T] <:< manifest[AnyVal]
  ) foreach (assert(_, "assertAnyVal"))

  def assertSameType[T: Manifest, U: Manifest] = assert(typeCompare[T, U] == SAME, "assertSameType")
  def assertSuperType[T: Manifest, U: Manifest] = assert(typeCompare[T, U] == SUPER, "assertSuperType")
  def assertSubType[T: Manifest, U: Manifest] = assert(typeCompare[T, U] == SUB, "assertSubType")
  def assertNoRelationship[T: Manifest, U: Manifest] = assert(typeCompare[T, U] == NONE, "assertNoRelationship")

  def testVariancesVia[T: Manifest, U: Manifest] = assert(
    typeCompare[T, U] == SUB &&
    showsCovariance[T, U, List] &&
    showsInvariance[T, U, Set],
    "testVariancesVia"
  )

  def runAllTests = {
    assertAnyVal[AnyVal]
    assertAnyVal[Unit]
    assertAnyVal[Int]
    assertAnyVal[Double]
    assertAnyVal[Boolean]
    assertAnyVal[Char]

    assertAnyRef[AnyRef]
    assertAnyRef[java.lang.Object]
    assertAnyRef[java.lang.Integer]
    assertAnyRef[java.lang.Double]
    assertAnyRef[java.lang.Boolean]
    assertAnyRef[java.lang.Character]
    assertAnyRef[String]
    assertAnyRef[scala.List[String]]
    assertAnyRef[scala.List[_]]

    // variance doesn't work yet
    // testVariancesVia[String, Any]
    // testVariancesVia[String, AnyRef]

    assertSubType[List[String], List[Any]]
    assertSubType[List[String], List[AnyRef]]
    assertNoRelationship[List[String], List[AnyVal]]

    assertSubType[List[Int], List[Any]]
    assertSubType[List[Int], List[AnyVal]]
    assertNoRelationship[List[Int], List[AnyRef]]

    // Nothing
    assertSubType[Nothing, Any]
    assertSubType[Nothing, AnyVal]
    assertSubType[Nothing, AnyRef]
    assertSubType[Nothing, String]
    assertSubType[Nothing, List[String]]
    assertSubType[Nothing, Null]
    assertSameType[Nothing, Nothing]

    // Null
    assertSubType[Null, Any]
    assertNoRelationship[Null, AnyVal]
    assertSubType[Null, AnyRef]
    assertSubType[Null, String]
    assertSubType[Null, List[String]]
    assertSameType[Null, Null]
    assertSuperType[Null, Nothing]

    // Any
    assertSameType[Any, Any]
    assertSuperType[Any, AnyVal]
    assertSuperType[Any, AnyRef]
    assertSuperType[Any, String]
    assertSuperType[Any, List[String]]
    assertSuperType[Any, Null]
    assertSuperType[Any, Nothing]

    // Misc unrelated types
    assertNoRelationship[Unit, AnyRef]
    assertNoRelationship[Unit, Int]
    assertNoRelationship[Int, Long]
    assertNoRelationship[Boolean, String]
    assertNoRelationship[List[Boolean], List[String]]
    assertNoRelationship[Set[Boolean], Set[String]]
  }

  def main(args: Array[String]): Unit = runAllTests
}
