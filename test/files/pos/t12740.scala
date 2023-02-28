class C(x: NoSuchElementException)

// available in java.lang
package object K extends Cloneable

package object XX extends Serializable
package object XY extends NoSuchElementException

object XZ extends NoSuchElementException

package object Y {
  type NSE = java.util.NoSuchElementException
}
package Z {
  import Y._
  class C extends NSE
}
