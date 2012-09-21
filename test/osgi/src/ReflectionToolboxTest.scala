package tools.test.osgi
package reflection
package toolbox
 
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


class C {
  val f1 = 2
}

@RunWith(classOf[JUnit4TestRunner])
@ExamReactorStrategy(Array(classOf[AllConfinedStagedReactorFactory]))
class ReflectionToolBoxTest extends ScalaOsgiHelper {

  @Configuration
  def config(): Array[exam.Option] = 
    standardOptions

 
  @Test
  def basicMirrorThroughOsgi(): Unit = {
    // Note - this tries to make sure when pulling a toolbox, we get the compiler.
    import scala.reflect.runtime.universe._
    import scala.tools.reflect.ToolBox
    val cm = runtimeMirror(classOf[C].getClassLoader)
    val tb = cm.mkToolBox()
    val im = cm.reflect(new C)
    val tree = tb.parse("1 to 3 map (_+1)")
    val eval = tb.eval(tree)
    assertEquals(Vector(2, 3, 4), eval)
    assertEquals("Evaluate expression using local class.", 
      2,
      tb.eval(tb.parse("(new tools.test.osgi.reflection.toolbox.C).f1")))
 }
}
