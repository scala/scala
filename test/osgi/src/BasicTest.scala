package tools.test.osgi
 
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
class BasicTest extends ScalaOsgiHelper {
  @Configuration
  def config(): Array[exam.Option] = {
    // TODO - Find scala bundles.
    standardOptions
  }
 
  @Test
  def everythingLoads(): Unit = ()
}
