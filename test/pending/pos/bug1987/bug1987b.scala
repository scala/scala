// compile with bug1987a.scala

package bug.packageb
// Note that the overloading works if instead of being in the package we import it:
// replace the above line with import bug.packageb._

class Client {
  val x = func(1) // doesn't compile: type mismatch; found: Int(1) required: String
  val y = func("1") // compiles
}
