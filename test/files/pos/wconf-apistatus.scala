// scalac: -Werror -Wconf:cat=api-may-change&origin=foo\..*&since>0.999:silent

import scala.annotation.{ apiStatus, apiStatusCategory, apiStatusDefaultAction }
import scala.annotation.meta._
import apiStatus._

package foo {
  object syntax {
    @apiMayChange(
      "should DSL is incubating, and future compatibility is not guaranteed",
      since = "foo-lib 1.0",
    )
    implicit class ShouldDSL(s: String) {
      def should(o: String): Unit = ()
    }
  }

  /**
   * Demo of custom API status annotation.
   */
  @apiStatusCategory(apiStatus.Category.ApiMayChange)
  @apiStatusDefaultAction(apiStatus.Action.Warning)
  @companionClass @companionMethod
  final class apiMayChange(
    message: String,
    since: String = "",
  ) extends apiStatus(message, since = since)
}

object Test1 {
  import foo.syntax._
  "bar" should "something"
}
