//> using options -Xdev -Werror
// Was: "warning: !!! base trait Serializable not found in basetypes of object Person. This might indicate incorrect caching of TypeRef#parents."
// under -Xdev
class Test {
  def apply = {
    case class Person(name: String)
    val x = Person("")
    Person.getClass
    x.name
  }
}
