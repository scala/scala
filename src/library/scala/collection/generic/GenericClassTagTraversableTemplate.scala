/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package generic

import mutable.Builder
import scala.annotation.unchecked.uncheckedVariance
import scala.language.higherKinds
import scala.reflect.ClassTag

/** This trait represents collections classes which require class
 *  tags for their element types.
 *
 *  @author Aleksandar Prokopec
 *  @since 2.8
 */
trait GenericClassTagTraversableTemplate[+A, +CC[X] <: Traversable[X]] extends HasNewBuilder[A, CC[A] @uncheckedVariance] {
  implicit protected[this] val tag: ClassTag[A]
  def classTagCompanion: GenericClassTagCompanion[CC]
  def genericClassTagBuilder[B](implicit tag: ClassTag[B]): Builder[B, CC[B]] = classTagCompanion.newBuilder[B]
  @deprecated("use classTagCompanion instead", "2.10.0")
  def classManifestCompanion: GenericClassManifestCompanion[CC] = classTagCompanion
  @deprecated("use genericClassTagBuilder instead", "2.10.0")
  def genericClassManifestBuilder[B](implicit manifest: ClassManifest[B]): Builder[B, CC[B]] = genericClassTagBuilder[B](manifest)
}
