//> using jvm 9+

import java.util.concurrent._
import scala.jdk.FutureConverters._

object Test extends App {
  val cf = CompletableFuture.completedStage("42")
  assert("42" == cf.asScala.value.get.get)
}
