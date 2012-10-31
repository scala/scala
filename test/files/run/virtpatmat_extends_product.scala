object Test extends App {
  case class AnnotationInfo(a: String, b: Int) extends Product2[String, Int] {
    def _1 = a
    def _2 = b
  }

  // if we're not careful in unapplyTypeListFromReturnType, the generated unapply is
  // thought to return two components instead of one, since AnnotationInfo (the result of the unapply) is a Product2
  case class NestedAnnotArg(ai: AnnotationInfo)

  NestedAnnotArg(AnnotationInfo("a", 1)) match {
    case NestedAnnotArg(x) => println(x)
  }
}
