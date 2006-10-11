import scala.xml._
object Test extends Application {
  {
  var z:NodeSeq = null

  Console.println(z)
  Console.println(z==null)
  var x = new UnprefixedAttribute("foo","bar", Null)
  //x = new UnprefixedAttribute("foo", null:Seq[Node], x)
  Console.println(x.toString)
  Console.println(x.get("foo")) // Some(bar)
  Console.println(x("foo")) // bar

  val y = x.remove("foo")
  Console.println(y.toString)
  Console.println(y == Null) // true

  x = new UnprefixedAttribute("foo", z, x)
  Console.println(x.toString)
  Console.println(x.get("foo")) // None
  Console.println(x("foo")) // null
  }
}
