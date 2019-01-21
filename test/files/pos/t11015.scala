class Foo[A]

object ParserInput { implicit def apply(string: String): ParserInput = ??? }
trait ParserInput

// overloaded with default arg
class JsonParser(input: ParserInput, settings: String = null) { def this(input: ParserInput) = this(input, null) }

class Test {
  def append(a: Foo[String]): Unit = ()
  def append(a: Foo[String], as: Int*): Unit = ()

  append(new Foo)

  // needs implicit conversion from companion of expected type
  // the problem was that getParts in implicits did not consider OverloadedArgProto's underlying type;
  // the implemented solution is to normalize the expected type of a view before calling inferView
  new JsonParser("""{"key":1}{"key":2}""")
}
