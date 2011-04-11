/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection
package generic

abstract class ClassManifestTraversableFactory[CC[X] <: Traversable[X] with GenericClassManifestTraversableTemplate[X, CC]]
              extends GenericClassManifestCompanion[CC] {

  class GenericCanBuildFrom[A](implicit manif: ClassManifest[A]) extends CanBuildFrom[CC[_], A, CC[A]] {
    def apply(from: CC[_]) = from.genericClassManifestBuilder[A]
    def apply = newBuilder[A]
  }
}
