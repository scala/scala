package scala.tools.nsc

import annotation.unchecked.uncheckedVariance

package object tasty {
  implicit final class SafeEq[-T](private val t: T @uncheckedVariance) extends AnyVal {
    @inline final def ===(u: T): Boolean = t == u
    @inline final def !==(u: T): Boolean = t != u
  }
}
