package scala.reflect

import java.lang.{Class => jClass}

/** An `ErasureTag[T]` is a descriptor that is requested by the compiler every time
 *  when it needs to persist an erasure of a type.
 *
 *  Scala library provides a standard implementation of this trait,
 *  `TypeTag[T]` that carries the `java.lang.Class` erasure for arbitrary types.
 *
 *  However other platforms may reimplement this trait as they see fit
 *  and then expose the implementation via an implicit macro.
 *
 *  If you need to guarantee that the type does not contain
 *  references to type parameters or abstract types, use `ClassTag[T]`.
 *
 * @see [[scala.reflect.api.TypeTags]]
 */
@annotation.implicitNotFound(msg = "No ErasureTag available for ${T}")
trait ErasureTag[T] {
  /** Returns an erasure of type `T` */
  def erasure: jClass[_]
}
