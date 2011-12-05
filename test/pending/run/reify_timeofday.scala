import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    class DateError extends Exception

    /** Simulating properties in Scala
     *  (example 4.2.1 in ScalaReference.pdf)
     */
    class TimeOfDayVar {
      private var h, m, s: Int = 0

      def hours = h

      /** A method 'ident_=' is a setter for 'ident'. 'code.ident = ...' will
       *  be translated to a call to 'ident_='
       */
      def hours_= (h: Int) =
        if (0 <= h && h < 24) this.h = h
        else throw new DateError()

      def minutes = m
      def minutes_= (m: Int) =
        if (0 <= m && m < 60) this.m = m
        else throw new DateError()

      def seconds = s
      def seconds_= (s: Int) =
        if (0 <= s && s < 60) this.s = s
        else throw new DateError()
    }

    val d = new TimeOfDayVar
    d.hours = 8; d.minutes = 30; d.seconds = 0
    try { d.hours = 25 // throws a DateError exception
    } catch {
      case de: DateError => println("DateError")
      case e: Exception => println("Exception")
    }
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
