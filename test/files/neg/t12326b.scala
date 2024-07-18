//> using options -Werror -Wunused:imports -Wconf:site=p.T:s,origin=scala.collection.mutable.Map:s,origin=scala.collection.mutable.Set:s

package p

import scala.collection.mutable.{ListBuffer, Map, Set}

trait T {
  import scala.concurrent.ExecutionContext.Implicits._
}
