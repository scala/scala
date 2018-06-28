/** found in genericNest.jar, compiled from OuterTParams.java */

// bug #695
object ForceParse extends OuterTParams_1[AnyRef] {
  // Force import of HarderToParse<A>.InnerClass,
  // which has confusing method signature.
  var field: InnerClass = null
}

object Test extends App {
  ForceParse
}
