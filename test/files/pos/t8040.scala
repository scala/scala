
object Test {
  implicit class C(val sc: StringContext) {                 // no warn unused sc
    def c(args: Any*): String = "c?" + args.mkString(",")   // would warn unused args
  }
}
