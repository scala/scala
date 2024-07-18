//> using options -Werror -Wunused:imports -Wconf:site=p.T:s

package p

import scala.collection.mutable._

trait T {
  import scala.concurrent.ExecutionContext.Implicits._
}
