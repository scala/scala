import scala.annotation.{ apiStatus, apiStatusCategory, apiStatusDefaultAction }
import scala.annotation.meta._

package foo {
  object syntax {
    @apiStatus(
      "method <<= is removed; use := syntax instead",
      category = apiStatus.Category.ForRemoval,
      since = "foo-lib 1.0",
      defaultAction = apiStatus.Action.Error,
    )
    def <<=() = ???

    @apiMayChange("should DSL is incubating, and future compatibility is not guaranteed")
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
  <<=()

  "bar" should {
    "something"
  }
}
