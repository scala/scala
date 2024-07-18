//> using options -Werror -Wunused:imports -Wconf:origin=scala.collection.mutable._:s,origin=scala.concurrent.ExecutionContext.Implicits._:s

import scala.collection.mutable._

trait T {
  import scala.concurrent.ExecutionContext.Implicits._
}
