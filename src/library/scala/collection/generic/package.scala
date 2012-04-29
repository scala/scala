package scala.collection
import generic.CanBuildFrom

package object generic {
  type CanBuild[-Elem, +To] = CanBuildFrom[Nothing, Elem, To]

  /** The type of conversions from a collection representation type
   *  `Repr` to its corresponding GenTraversableLike.
   *  @see [[scala.collection.generic.FromRepr]]
   */
  type HasElem[Repr, A] = Repr => GenTraversableLike[A, Repr]

  @deprecated("use ArrayTagTraversableFactory instead", "2.10.0")
  type ClassManifestTraversableFactory[CC[X] <: Traversable[X] with GenericClassManifestTraversableTemplate[X, CC]] = ArrayTagTraversableFactory[CC]

  @deprecated("use GenericArrayTagCompanion instead", "2.10.0")
  type GenericClassManifestCompanion[+CC[X] <: Traversable[X]] = GenericArrayTagCompanion[CC]

  @deprecated("use GenericArrayTagTraversableTemplate instead", "2.10.0")
  type GenericClassManifestTraversableTemplate[+A, +CC[X] <: Traversable[X]] = GenericArrayTagTraversableTemplate[A, CC]
}