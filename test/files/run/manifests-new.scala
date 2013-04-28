

import scala.language.{ higherKinds, postfixOps }
import scala.reflect.runtime.universe._

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
            implicit ev1: TypeTag[T], ev2: TypeTag[U], ev3: TypeTag[CC[T]], ev4: TypeTag[CC[U]]) {

    def elements = List(ev1.tpe <:< ev2.tpe, ev2.tpe <:< ev1.tpe)
    def containers = List(ev3.tpe <:< ev4.tpe, ev4.tpe <:< ev3.tpe)

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

  def showsCovariance[T, U, CC[_]](implicit ev1: TypeTag[T], ev2: TypeTag[U], ev3: TypeTag[CC[T]], ev4: TypeTag[CC[U]]) =
    new VarianceTester[T, U, CC](CO) showsExpectedVariance

  def showsInvariance[T, U, CC[_]](implicit ev1: TypeTag[T], ev2: TypeTag[U], ev3: TypeTag[CC[T]], ev4: TypeTag[CC[U]]) =
    new VarianceTester[T, U, CC](IN) showsExpectedVariance

  def showsContravariance[T, U, CC[_]](implicit ev1: TypeTag[T], ev2: TypeTag[U], ev3: TypeTag[CC[T]], ev4: TypeTag[CC[U]]) =
    new VarianceTester[T, U, CC](CONTRA) showsExpectedVariance

  def typeCompare[T, U](implicit ev1: TypeTag[T], ev2: TypeTag[U]) = (ev1.tpe <:< ev2.tpe, ev2.tpe <:< ev1.tpe) match {
    case (true, true)   => SAME
    case (true, false)  => SUB
    case (false, true)  => SUPER
    case (false, false) => NONE
  }

  def assertAnyRef[T: TypeTag] = List(
    typeOf[T] <:< typeOf[Any],
    typeOf[T] <:< typeOf[AnyRef],
    !(typeOf[T] <:< typeOf[AnyVal])
  ) foreach (assert(_, "assertAnyRef"))

  def assertAnyVal[T: TypeTag] = List(
    typeOf[T] <:< typeOf[Any],
    !(typeOf[T] <:< typeOf[AnyRef]),
    typeOf[T] <:< typeOf[AnyVal]
  ) foreach (assert(_, "assertAnyVal"))

  def assertSameType[T: TypeTag, U: TypeTag] = assert(typeCompare[T, U] == SAME, "assertSameType")
  def assertSuperType[T: TypeTag, U: TypeTag] = assert(typeCompare[T, U] == SUPER, "assertSuperType")
  def assertSubType[T: TypeTag, U: TypeTag] = assert(typeCompare[T, U] == SUB, "assertSubType")
  def assertNoRelationship[T: TypeTag, U: TypeTag] = assert(typeCompare[T, U] == NONE, "assertNoRelationship")

  def testVariancesVia[T: TypeTag, U: TypeTag] = assert(
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
