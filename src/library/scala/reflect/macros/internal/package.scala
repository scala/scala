package scala.reflect.macros

import scala.reflect.base.{Universe => BaseUniverse}
import scala.reflect.ClassTag

package object internal {
  // should all be removed once I re-deploy the starr
  def materializeClassTag[T](u: BaseUniverse): ClassTag[T] = ???
  def materializeWeakTypeTag: Nothing = ???
  def materializeTypeTag: Nothing = ???
}
