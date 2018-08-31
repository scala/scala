// scalac: -deprecation -Xmigration:2.10 -Xfatal-warnings
//
// showing that multiple warnings at same location are reported
//
package scala.test
import scala.annotation._

class A {
  implicit class BooleanOps(val b: Boolean) {
    @deprecated("bobo", since="2.11.0")
    @migration("Used to return 5", changedIn="2.12.6")
    def bippy() = 42
  }
  def f = (null == null).bippy
  def g = true.bippy
}

/*

$ ~/scala-2.10.4/bin/scalac -d /tmp -Xmigration:2.10 -deprecation test/files/neg/t6120.scala 
test/files/neg/t6120.scala:14: warning: method bippy in class BooleanOps is deprecated: bobo
  def f = (null == null).bippy
                         ^
test/files/neg/t6120.scala:14: warning: comparing values of types Null and Null using `==' will always yield true
  def f = (null == null).bippy
                ^
test/files/neg/t6120.scala:15: warning: method bippy in class BooleanOps is deprecated: bobo
  def g = true.bippy
               ^
three warnings found

 */
