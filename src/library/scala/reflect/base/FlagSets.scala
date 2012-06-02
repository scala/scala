package scala.reflect
package base

trait FlagSets { self: Universe =>

  /** An abstract type representing sets of flags that apply to definition trees and symbols */
  type FlagSet

  /** A tag that preserves the identity of the `FlagSet` abstract type from erasure.
   *  Can be used for pattern matching, instance tests, serialization and likes.
   */
  implicit val FlagSetTag: ClassTag[FlagSet]

  /** The empty set of flags */
  val NoFlags: FlagSet
}

