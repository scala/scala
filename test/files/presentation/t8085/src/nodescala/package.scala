import scala.Some // <-- if you move the import *inside* the package object, then it all works fine!!

package object nodescala {
  implicit class StringOps(val f: String) {
    def rich = 0
  }
}
