/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package nsc
package incremental

/**
  * A cached map from class symbols to the next non-local enclosing class.
  * A non-local class is a class that is owned by either a package or another non-local class.
  *
  * Let's consider an example of an owner chain:
  *
  *   pkg1 <- pkg2 <- class A <- object B <- class C <- def foo <- class Foo <- class Bar
  *
  * For an object, we work with its `moduleClass` so we can refer to everything as classes.
  *
  * Classes A, B, C are non-local so they are mapped to themselves. Classes Foo and Bar are local
  * because they are defined within method `foo`, so they are mapped to `C`.
  *
  * Method `isLocal` returns whether given class is local.
  *
  * This map is populated during the [[Dependency]] phase. It is used during the backend when
  * creating a classfile, to decide whether `callback.generatedLocalClass` should be invoked.
  */
class EnclosingNonLocalClassMap[G <: ZincGlobal](val global: G) {
  import global._

  private[this] val cache = perRunCaches.newMap[Symbol, Symbol]()

  /**
    * If `s` is a local class, return the inner-most non-local enclosing class.
    * Otherwise, return `s`.
    */
  def apply(s: Symbol): Symbol = {
    assert(s.isClass, s"The ${s.fullName} is not a class.")
    cache.getOrElseUpdate(s, lookupNonLocal(s))
  }

  /**
    * Returns `true` if `s` is a local class, `false` otherwise.
    */
  def isLocal(s: Symbol): Boolean = {
    assert(s.isClass, s"The ${s.fullName} is not a class.")
    apply(s) != s
  }

  private def lookupNonLocal(s: Symbol): Symbol = {
    val origOwner = s.originalOwner // originalOwner makes it work after lambdalift / flatten
    if (origOwner.isPackageClass) s
    else if (origOwner.isClass) {
      val nonLocalForOwner = apply(origOwner)
      // the s is owned by a non local class so s is non local
      if (nonLocalForOwner == origOwner) s
      // otherwise the inner most non local class is the same as for its owner
      else nonLocalForOwner
    } else apply(origOwner.enclClass)
  }
}
