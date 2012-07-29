package scala.tools.scalap
package scalax

package object rules {
  implicit lazy val higherKinds         = language.higherKinds
  implicit lazy val postfixOps          = language.postfixOps
  implicit lazy val implicitConversions = language.implicitConversions
  implicit lazy val reflectiveCalls     = language.reflectiveCalls
}
