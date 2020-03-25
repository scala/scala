// scalac: -Werror -Wconf:cat=api-may-change&origin=foo\..*&since>0.999:silent

import scala.annotation.apiStatus, apiStatus._

package foo {
  object syntax {
    @apiStatus(
      "should DSL is incubating, and future compatibility is not guaranteed",
      category = Category.ApiMayChange,
      since = "foo-lib 1.0",
    )
    implicit class ShouldDSL(s: String) {
      def should(o: String): Unit = ()
    }
  }
}

object Test1 {
  import foo.syntax._
  "bar" should "something"
}
