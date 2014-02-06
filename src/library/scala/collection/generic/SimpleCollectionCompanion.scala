package scala
package collection
package generic

import scala.language.higherKinds
import mutable.Builder

/** A class for companions which attempts to minimize the boilerplate requirements.
 *  It's not intended for general use.
 */
private[scala] abstract class SimpleCollectionCompanion[CC[X]] {
  def newBuilder[A] : Builder[A, CC[A]]
  private object ReusableCBF extends CanBuildFrom[CC[_], Nothing, CC[Nothing]] {
    def apply(from: CC[_]) = newBuilder
    def apply()            = newBuilder
  }
  implicit def canBuildFrom[A]: CanBuildFrom[CC[_], A, CC[A]] = ReusableCBF.asInstanceOf[CanBuildFrom[CC[_], A, CC[A]]]
}
