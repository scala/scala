
case class Foo(s: String)

object Test {
  def f1 = {
    import scala.collection.JavaConversions._
    val map: Map[Foo, String] = Map(Foo("a") -> "a", Foo("b") -> "b")
    val v = map.get("a")  // should be a type error, actually returns null
  }
  def f2 = {
    import scala.collection.convert.wrapAsScala._
    val map: Map[Foo, String] = Map(Foo("a") -> "a", Foo("b") -> "b")
    val v = map.get("a")  // now this is a type error
  }
  def f3 = {
    import scala.collection.convert.wrapAsJava._
    val map: Map[Foo, String] = Map(Foo("a") -> "a", Foo("b") -> "b")
    val v = map.get("a")
  }
}
