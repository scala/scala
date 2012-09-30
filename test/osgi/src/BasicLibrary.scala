package tools.test.osgi
package libonly
 
import org.junit.Assert._
import org.ops4j.pax.exam.CoreOptions._
 
import org.junit.Test
import org.junit.runner.RunWith
import org.ops4j.pax.exam
import org.ops4j.pax.exam.junit.{
  Configuration,
  ExamReactorStrategy,
  JUnit4TestRunner
}
import org.ops4j.pax.exam.spi.reactors.AllConfinedStagedReactorFactory
import org.ops4j.pax.swissbox.framework.ServiceLookup
import org.osgi.framework.BundleContext



@RunWith(classOf[JUnit4TestRunner])
@ExamReactorStrategy(Array(classOf[AllConfinedStagedReactorFactory]))
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
