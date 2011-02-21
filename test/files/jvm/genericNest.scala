/** found in genericNest.jar, compiled from OuterTParams.java */
import nestpkg._;

// bug #695
object ForceParse extends OuterTParams[AnyRef] {
  // Force import of HarderToParse<A>.InnerClass,
  // which has confusing method signature.
  var field: InnerClass = null
}

object Test extends App {
  ForceParse
}
