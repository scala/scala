import scala.tools.nsc.doc.base._
import scala.tools.nsc.doc.model._
import scala.tools.partest.ScaladocModelTest

object Test extends ScaladocModelTest {

  override def resourceFile = "SI-3314.scala"

  // no need for special settings
  def scaladocSettings = "-feature"

  def testModel(rootPackage: Package) = {
    // get the quick access implicit defs in scope (_package(s), _class(es), _trait(s), object(s) _method(s), _value(s))
    import access._

    // just need to check the member exists, access methods will throw an error if there's a problem
    val base = rootPackage._package("scala")._package("test")._package("scaladoc")



    // test1

    val test1 = base._package("test1")
    val test1Value = test1._class("Enum")._method("Value").resultType
    assert(test1Value.name == "Value",       test1Value.name + " == Value")
    assert(test1Value.refEntity.size == 1, test1Value.refEntity.size + " == 1")

    val test1Constants = test1._object("Constants")._method("a").resultType
    assert(test1Constants.name == "Value", test1Constants.name + " == Value")
    assert(test1Constants.refEntity.size == 1, test1Constants.refEntity.size + " == 1")
    assert(test1Constants.refEntity(0)._1 == LinkToMember(test1._object("Constants")._class("Value"), test1._object("Constants")),
           test1Constants.refEntity(0)._1 + " == LinkToMember(test1.Enum.Value)")


    // test2

    val test2 = base._package("test2")
    def testDefinition(doc: DocTemplateEntity) = {
      for (day <- List("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) {
        assert(doc._value(day).resultType.name == "Value",
               doc._value(day).resultType.name + " == Value")
        assert(doc._value(day).resultType.refEntity.size == 1,
               doc._value(day).resultType.refEntity.size + " == 1")
        assert(doc._value(day).resultType.refEntity(0)._1 == LinkToMember(doc._classMbr("Value"), doc),
               doc._value(day).resultType.refEntity(0)._1 + " == LinkToMember(" + doc.qualifiedName + ".Value)")
      }
    }
    testDefinition(test2._trait("WeekDayTrait"))
    testDefinition(test2._class("WeekDayClass"))
    testDefinition(test2._object("WeekDayObject"))

    def testUsage(doc: DocTemplateEntity) = {
      val ValueInClass = test2._class("WeekDayClass")._classMbr("Value")
      val ValueInTrait = test2._trait("WeekDayTrait")._classMbr("Value")
      val ValueInObject = test2._object("WeekDayObject")._classMbr("Value")
      val WeekDayInObject = test2._object("WeekDayObject")._member("WeekDay")

      val expected = List(
        ("isWorkingDay1", "WeekDayClass.Value",   ValueInClass),
        ("isWorkingDay2", "WeekDayClass.Value",   ValueInClass),
        ("isWorkingDay3", "WeekDayTrait.Value",   ValueInTrait),
        ("isWorkingDay4", "WeekDayTrait.Value",   ValueInTrait),
        ("isWorkingDay5", "WeekDayObject.Value",   ValueInObject),
        ("isWorkingDay6", "WeekDay", WeekDayInObject),
        ("isWorkingDay7", "WeekDayObject.Value",   ValueInObject),
        ("isWorkingDay8", "WeekDay", WeekDayInObject),
        ("isWorkingDay9", "WeekDayObject.Value",   ValueInObject))

      for ((method, name, ref) <- expected) {
        assert(doc._method(method).valueParams(0)(0).resultType.name == name,
               doc._method(method).valueParams(0)(0).resultType.name + " == " + name + " (in " + doc + "." + method + ")")
        assert(doc._method(method).valueParams(0)(0).resultType.refEntity.size == 1,
               doc._method(method).valueParams(0)(0).resultType.refEntity.size + " == " + 1 + " (in " + doc + "." + method + ")")
        assert(doc._method(method).valueParams(0)(0).resultType.refEntity(0)._1 == LinkToMember(ref, ref.inTemplate),
               doc._method(method).valueParams(0)(0).resultType.refEntity(0)._1 + " == LinkToMember(" + ref.qualifiedName + ") (in " + doc + "." + method + ")")
      }
    }
    testUsage(test2._object("UserObject"))
    testUsage(test2._class("UserClass"))
    testUsage(test2._trait("UserTrait"))


    // test3
    val test3 = base._package("test3")
    val foo = test3._method("foo")
    assert(foo.valueParams(0)(0).resultType.name == "T",
           foo.valueParams(0)(0).resultType.name + " == T")
    val bar = test3._method("bar")
    assert(bar.valueParams(0)(0).resultType.name == "(AnyRef { type Lambda[X] <: Either[A,X] })#Lambda[String]",
           bar.valueParams(0)(0).resultType.name + " == (AnyRef { type Lambda[X] <: Either[A,X] })#Lambda[String]")
  }
}
