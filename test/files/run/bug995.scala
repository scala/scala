object Test extends Application {
  def foo(v: Any): String = v match {
    case s: Seq[_] =>
      "Seq"
    // see Burak's hack in object Seq.unapplySeq
    //case a: AnyRef if runtime.ScalaRunTime.isArray(a) =>
    //  "Array"
    case _ =>
      v.toString
  }
  Console.println(foo(Array(0)))
}
