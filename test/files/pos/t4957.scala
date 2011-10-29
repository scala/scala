// a.scala
// Sat Oct 29 10:06:51 PDT 2011

package simple

import scala.{Double=>double, Int=>int}

/**
 * @author Christoph Radig
 */

trait Vector {

        def xd: double
        def yd: double
}

object Vector {

        def apply(x: double, y: double) = Double(x, y)
        def apply(x: int, y: int) = Int(x, y)

        trait Companion[@specialized(double, int) T] {

                type I <: Instance[T]

                def apply(x: T, y: T): I  // I (= this.type#I) or this.I?

                lazy val zero: I = apply(numeric.zero, numeric.zero)

                val numeric: Numeric[T]
        }

        trait Instance[@specialized(double, int) T] extends Vector {

                type C <: Companion[T]
                def companion: C

                def numeric: Numeric[T] = companion.numeric

                val x: T
                val y: T

                def xd = numeric.toDouble(x)
                def yd = numeric.toDouble(y)

                def + (that: C#I): C#I = companion(numeric.plus(this.x, that.x), numeric.plus(this.y, that.y))
                def - (that: C#I): C#I = companion(numeric.minus(this.x, that.x), numeric.minus(this.y, that.y))

                /**
                 * scalar multiplication
                 */
                def * (scalar: T): C#I = companion(numeric.times(this.x, scalar), numeric.times(this.y, scalar))
        }

        object Double extends Companion[double] {

                type I = Double

                def apply(x: double, y: double) = new Double(x, y)

                val numeric = Numeric.DoubleIsFractional
        }

        final class Double(val x: double, val y: double) extends Instance[double] {

                type C = Double.type
                def companion = Double

                @inline override def xd = x
                @inline override def yd = y
        }


        object Int extends Companion[int] {

                type I = Int

                def apply(x: int, y: int) = new Int(x, y)

                val numeric = Numeric.IntIsIntegral
        }

        final class Int(val x: int, val y: int) extends Instance[int] {

                type C = Int.type
                def companion = Int
        }
}
