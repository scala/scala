import language.dynamics

class C(v: Any) extends Dynamic {
  def selectDynamic[T](n: String): Option[T] = Option(v.asInstanceOf[T])
  def applyDynamic[T](n: String)(): Option[T] = Option(v.asInstanceOf[T])
}

object Test extends App {
  // this should be converted to
  // C(42).selectDynamic[String]("foo").get
  // causing a compile error.

  // but, before fixing SI-6663, became
  // C(42).selectDynamic("foo").get, ignoring
  // the [String] type parameter
  var v = new C(42).foo[String].get :Int
  println(v)
}

