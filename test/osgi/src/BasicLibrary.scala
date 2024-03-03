package tools.test.osgi
package libonly
 
import org.junit.Assert._
 
import org.junit.Test
import org.junit.runner.RunWith
import org.ops4j.pax.exam
import org.ops4j.pax.exam.Configuration
import org.ops4j.pax.exam.junit.PaxExam
import org.ops4j.pax.exam.spi.reactors.{ ExamReactorStrategy, PerMethod }

@RunWith(classOf[PaxExam])
@ExamReactorStrategy(Array(classOf[PerMethod]))
class BasicLibraryTest extends ScalaOsgiHelper {
  @Configuration
  def config(): Array[exam.Option] =
    justCoreLibraryOptions
 
  @Test
  def everythingLoads(): Unit = {
     // Note - This tests sun.misc usage.
     import scala.concurrent._
     import scala.concurrent.duration.Duration.Inf
     import ExecutionContext.Implicits._
     val x = Future(2) map (_ + 1)
     assertEquals(3, Await.result(x, Inf))
  }
}
