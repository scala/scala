package xsbt

import collection.mutable.Map

/**
  * A memoized function that maps a local class to its inner most non local class
  * owner.
  *
  * Let's consider an example of an owner chain:
  *
  *   pkg1 <- pkg2 <- class A <- object B <- class C <- def foo <- class Foo <- class Bar
  *
  * For an object, we work with its `moduleClass` so we can refer to everything as classes.
  *
  * Classes A, B, C are non local so they are mapped to themselves. Classes Foo and Bar are local because
  * they are defined within method `foo`.
  *
  * Let's define non local class more precisely. A non local class is a class that is owned by either a package
  * or another non local class. This gives rise to a recursive definition of non local class that is used for
  * implementation of the mapping.
  *
  * Thanks to memoization, the amortized cost of a lookup is O(1). We amortize over lookups for all class symbols
  * in the current compilation run.
  *
  * NOTE: This class doesn't extend Function1 because I couldn't get path-dependent types right.
  */
class LocalToNonLocalClass[G <: CallbackGlobal](val global: G) {
  import global._
  private val cache: Map[Symbol, Symbol] = perRunCaches.newMap()
  def apply(s: Symbol): Symbol = {
    assert(s.isClass, s"The ${s.fullName} is not a class.")
    cache.getOrElseUpdate(s, resolveNonLocal(s))
  }
  private def resolveNonLocal(s: Symbol): Symbol = {
    assert(phase.id <= sbtDependency.ownPhase.id,
      s"Resolution of non local classes works up to sbtDependency phase but we're at ${phase.name}")
    lookupNonLocal(s)
  }
  private def lookupNonLocal(s: Symbol): Symbol = {
    if (s.owner.isPackageClass) s
    else if (s.owner.isClass) {
      val nonLocalForOwner = apply(s.owner)
      // the s is owned by a non local class so s is non local
      if (nonLocalForOwner == s.owner) s
      // otherwise the inner most non local class is the same as for its owner
      else nonLocalForOwner
    } else apply(s.owner.enclClass)
  }
}
