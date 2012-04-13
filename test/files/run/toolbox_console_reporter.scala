import scala.reflect.mirror._

object Test extends App {
  // todo. cannot test this unfortunately, because ConsoleReporter grabs Console.out too early
  // todo. and isn't affected by Console.setOut employed by partest to intercept output

  //val toolbox = mkToolBox(reporter = mkConsoleReporter(), options = "-deprecation")
  //toolbox.runExpr(reify{
  //  object Utils {
  //    @deprecated("test", "2.10.0")
  //    def foo { println("hello") }
  //  }
  //
  //  Utils.foo
  //})
}
