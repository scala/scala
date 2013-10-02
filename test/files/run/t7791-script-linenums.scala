
import scala.tools.partest.ScriptTest

object Test extends ScriptTest {
  object ExceptionLine {
    def unapply(e: Exception) = Some(e.getStackTrace()(0).getLineNumber)
  }
  override def show() = {
    import util._
    Try(super.show()) match {
      case Failure(ExceptionLine(7)) => ()
      case Failure(e) => e.printStackTrace()
      case Success(_) => Console println "Expected error"
    }
  }
}
