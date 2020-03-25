import scala.annotation.apiStatus

package foo {
  object syntax {
    @apiStatus(
      "method <<= is removed; use := syntax instead",
      category = apiStatus.Category.ForRemoval,
      since = "foo-lib 1.0",
      defaultAction = apiStatus.Action.Error,
    )
    def <<=() = ???

    @apiStatus(
      "should DSL is incubating, and future compatibility is not guaranteed",
      category = apiStatus.Category.ApiMayChange,
      since = "foo-lib 1.0",
    )
    implicit class ShouldDSL(s: String) {
      def should(o: String): Unit = ()
    }
  }
}

object Test1 {
  import foo.syntax._
  <<=()

  "bar" should {
    "something"
  }
}
