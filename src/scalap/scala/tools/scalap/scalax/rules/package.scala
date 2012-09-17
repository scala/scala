package scala.tools.scalap
package scalax

package object rules {
  implicit lazy val higherKinds         = scala.language.higherKinds
  implicit lazy val postfixOps          = scala.language.postfixOps
  implicit lazy val implicitConversions = scala.language.implicitConversions
  implicit lazy val reflectiveCalls     = scala.language.reflectiveCalls
}
