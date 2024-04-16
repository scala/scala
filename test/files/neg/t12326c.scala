//> using options -Werror -Wunused:imports -Wconf:origin=scala.collection.mutable.*:s

package p

import scala.collection.mutable.{ListBuffer, Map, Set}

trait T {
  import scala.concurrent.ExecutionContext.Implicits._
}
