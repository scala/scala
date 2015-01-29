package tools.test.osgi
 
import org.junit.Assert._
import org.ops4j.pax.exam.CoreOptions._
 
import org.junit.Test
import org.junit.runner.RunWith
import org.ops4j.pax.exam
import org.ops4j.pax.exam.Configuration
import org.ops4j.pax.exam.junit.PaxExam
import org.ops4j.pax.exam.spi.reactors.{ ExamReactorStrategy, PerMethod }
import org.ops4j.pax.swissbox.tracker.ServiceLookup
import org.osgi.framework.BundleContext





@RunWith(classOf[PaxExam])
@ExamReactorStrategy(Array(classOf[PerMethod]))
class BasicTest extends ScalaOsgiHelper {
  @Configuration
  def config(): Array[exam.Option] = {
    // TODO - Find scala bundles.
    standardOptions
  }
 
  @Test
  def everythingLoads(): Unit = ()
}
