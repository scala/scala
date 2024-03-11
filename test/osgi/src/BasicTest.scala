package tools.test.osgi
 
import org.junit.Test
import org.junit.runner.RunWith
import org.ops4j.pax.exam
import org.ops4j.pax.exam.Configuration
import org.ops4j.pax.exam.junit.PaxExam
import org.ops4j.pax.exam.spi.reactors.{ ExamReactorStrategy, PerMethod }

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
