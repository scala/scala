import scala.language.{ dynamics, implicitConversions }

class ID(s: String)

class DynamicFields extends scala.Dynamic {
  def updateDynamic(name: ID)(value: Any): Unit = ???
}

object BugUpdateDynamic {
  implicit def string2id(s: String): ID = new ID(s)

  def explicitly(): Unit = {
    val o = new DynamicFields
    o.updateDynamic("foo")("bar")
  }
  def bug(): Unit = {
    val o = new DynamicFields
    o.foo = "bar"
  }
}
