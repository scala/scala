/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.reflect


/** This type is required by the compiler and <b>should not be used in client code</b>.
  * A Manifest[T] is a descriptor for the type T.
  *  The object `Manifest' defines factory methods for manifests which still need to be implemented.
  *  Also still to be done are manifests for refinement types.
  */
object Manifest {

  /** Manifest for the singleton type `value.type'
   */
  def singleType[T](value: Any): Manifest[T] = null

  /** Manifest for the class type `clazz', where `clazz' is
   *  a top-level or static class
   */
  def classType[T](clazz: Predef.Class[_]): Manifest[T] = null

  /** Manifest for the class type `clazz[args]', where `clazz' is
   *  a top-level or static class
   */
  def classType[T](clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] = null

  /** Manifest for the class type `prefix # clazz'
   */
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_]): Manifest[T] = null

  /** Manifest for the class type `prefix # clazz[args]'
   */
  def classType[T](prefix: Manifest[_], clazz: Predef.Class[_], args: Manifest[_]*): Manifest[T] = null

  /** Manifest for the abstract type `prefix # name'
   */
  def abstractType[T](prefix: Manifest[_], name: String): Manifest[T] = null

  /** Manifest for the abstract type `prefix # name[args]'
   */
  def abstractType[T](prefix: Manifest[_], name: String, args: Manifest[_]*): Manifest[T] = null

  /** Manifest for the intersection type `parents_0 with ... with parents_n'
   */
  def intersectionType[T](parents: Manifest[_]*): Manifest[T] = null
}

/** This type is required by the compiler and <b>should not be used in client code</b>.
  * A Manifest[T] is a descriptor for the type T. */
abstract class Manifest[T]

