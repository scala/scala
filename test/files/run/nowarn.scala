
import scala.tools.nsc.{Driver, Global}
import scala.reflect.internal.util.NoPosition

// ensure that nowarn is respected by the default reporter

class Driven extends Driver {
  override protected def processSettingsHook(): Boolean = {
    settings.nowarn.value = true
    true
  }
  protected def newCompiler(): Global = Global(settings, reporter)
  override protected def doCompile(compiler: Global): Unit = reporter.warning(NoPosition, s"I can't do anything for ${reporter.getClass}.")
  def run(): Unit = process(Array("file.scala"))
}
object Test {
  def main(args: Array[String]): Unit = new Driven().run()
}
