
class C {
  def f = List(null: _*)
}
object Test extends App {
  import scala.tools.testkit.AssertUtil._
  val c = new C
  assertThrows[NullPointerException](c.f)
}
/*
java.lang.NullPointerException: Cannot invoke "scala.collection.IterableOnce.knownSize()" because "prefix" is null
        at scala.collection.immutable.List.prependedAll(List.scala:148)
        at scala.collection.immutable.List$.from(List.scala:685)
        at scala.collection.immutable.List$.from(List.scala:682)
        at scala.collection.IterableFactory.apply(Factory.scala:103)
        at scala.collection.IterableFactory.apply$(Factory.scala:103)
        at scala.collection.immutable.List$.apply(List.scala:682)
        at C.f(t12198.scala:3)

was exception caught in pickler:
error: erroneous or inaccessible type
*/
