import scala.tools.reflect._
import java.util.concurrent.Callable
import java.io.Closeable

object Test {
  // It'd be really nice about now if functions had a common parent.
  implicit def interfaceify(x: AnyRef): UniversalFn = UniversalFn(x)
  
  def runner(x: Runnable) = x.run()
  def caller[T](x: Callable[T]): T = x.call()
  def closer(x: Closeable) = x.close()
  
  def main(args: Array[String]): Unit = {
    var counter = 0
    val closure = () => {
      counter += 1
      println("Hi, thanks for calling: that makes " + counter + " times.")
      counter
    }

    val int1 = closure.as[Runnable]
    val int2 = closure.as[Callable[Int]]
    val int3 = closure.as[Closeable]
    
    runner(int1)
    caller(int2)
    closer(int3)
  }
}
