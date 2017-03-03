import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global

object Test {
  def main(args: Array[String]): Unit = {
    implicit val dummy: ExecutionContext = null
    require(scala.concurrent.ExecutionContext.Implicits.global ne null)
    require(implicitly[ExecutionContext] eq dummy)
  }
}