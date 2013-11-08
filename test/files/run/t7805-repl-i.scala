
import scala.tools.partest.{ ReplTest, Welcoming }
import scala.tools.nsc.{ GenericRunnerSettings, Settings }
import scala.tools.nsc.settings.MutableSettings

object Test extends ReplTest with HangingRepl with Welcoming {
  def script = testPath changeExtension "script"
  override def transformSettings(s: Settings) = s match {
    case m: MutableSettings =>
      val t = new GenericRunnerSettings(s.errorFn)
      m copyInto t
      t processArgumentString s"-i $script"
      t
    case _ => s
  }
  def code = "Console println Try(8)"
}

object Resulting {
  import scala.concurrent._
  import scala.concurrent.duration._
  implicit class AwaitResult[A](val f: Future[A]) extends AnyVal {
    def resultWithin(d: Duration): A = Await.result(f, d)
  }
}

/** Test that hangs the REPL.
 *  Usually that is the "before" case.
 */
trait HangingRepl extends ReplTest {
  import scala.language.postfixOps
  import scala.util._
  import scala.concurrent._
  import scala.concurrent.duration._
  import ExecutionContext.Implicits._
  import Resulting._
  def timeout = 120 seconds
  def hanging[A](a: =>A): A = Future(a) resultWithin timeout
  override def show() = Try(hanging(super.show())) recover {
    case e => e.printStackTrace()
  }
}
