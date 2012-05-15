import scala.tools.nsc._  

object Test {
  val testCode = <code>
    import java.lang.Thread.`yield`
    import scala.`package`.Throwable
    
    `yield`  
  </code>.text
  
  def main(args: Array[String]) = {
    val settings = new Settings()
    settings.classpath.value = System.getProperty("java.class.path")
    val repl = new interpreter.IMain(settings)
    repl.interpret(testCode)    
  }
}

