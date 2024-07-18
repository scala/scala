//> using options -Werror -Wunused:imports

import annotation._

@nowarn("origin=scala.concurrent.ExecutionContext.Implicits._")
trait T {
  import scala.concurrent.ExecutionContext.Implicits._
}
