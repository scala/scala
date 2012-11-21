import language.dynamics

class C(v: Any) extends Dynamic {
  def selectDynamic[T](n: String): Option[T] = Option(v.asInstanceOf[T])
  def applyDynamic[T](n: String)(): Option[T] = Option(v.asInstanceOf[T])
}

object Test extends App {
  // this should be converted to
  // C(42).selectDynamic[Int]("foo").get
  // but, before fixing SI-6663, became
  // C(42).selectDynamic[Nothing]("foo").get
  // leading to a ClassCastException
  var v = new C(42).foo[Int].get
  println(v)
}

