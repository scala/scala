
object Test {
  implicit class C(val sc: StringContext) {                 // no warn unused sc
    def c(args: Any*): String = "c?" + args.mkString(",")   // would warn unused args
  }

  def f(implicit x: DummyImplicit) = 42                     // no warn DummyImplicit


  def f(x: Int)(y: Int = 1) = x + y                         // no warn default getter

  def g(@deprecated("","") x: Int) = 42                     // no warn deprecated
}
