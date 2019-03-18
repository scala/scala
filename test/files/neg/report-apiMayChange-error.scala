// scalac: -Wconfig apiMayChange:foo.*:error

import scala.annotation.{ restricted, RestrictedLabel }

package foo {
  object syntax {
    @restricted("foo DSL is incubating", since = "foo-lib 1.0", label = RestrictedLabel.apiMayChange)
    implicit class FooDSL(s: String) {
      def should(o: String): Unit = ()
    }
  }
}

object Test1 {
  import foo.syntax._
  "bar" should "something"
}
