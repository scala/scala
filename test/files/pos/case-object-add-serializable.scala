// scalac: -Werror
// Was: "warning: !!! base trait Serializable not found in basetypes of object Person. This might indicate incorrect caching of TypeRef#parents."
class Test {
  def apply = {
    case class Person(name: String)
    val x = Person("")
    Person.getClass
    x.name
  }
}
