class DryRun {
  import scala.tools.nsc.{Global, Settings, CompilerCommand}
  import scala.tools.nsc.reporters.ConsoleReporter

  val settings = new Settings()
  settings.classpath.value = System.getProperty("java.class.path")
  val command = new CompilerCommand(List(), settings)
  val reporter = new ConsoleReporter(settings, scala.Console.in, new java.io.PrintWriter(new java.io.PrintStream(scala.Console.out)))
  object compiler extends Global(command.settings, reporter) {
   object test1
   lazy val test2 = 1
   object test3
  }
  def test {
    compiler.test1
    compiler.test2
    compiler.test3
    val run = new compiler.Run
    run compile command.files
  }
}

object Test {
    def main(args: Array[String]) {
        new DryRun().test
    }
}
