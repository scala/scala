import scala.tools.nsc._
import scala.tools.nsc.interpreter.shell.ReplReporterImpl

object Test {
  val testCode = """
    import java.lang.Thread.`yield`
    import scala.`package`.Throwable
    
    `yield`  
  """
  
  def main(args: Array[String]) {
    val settings = new Settings()
    settings.classpath.value = System.getProperty("java.class.path")
    val repl = new interpreter.IMain(settings, new ReplReporterImpl(settings))
    repl.interpret(testCode)    
  }
}

