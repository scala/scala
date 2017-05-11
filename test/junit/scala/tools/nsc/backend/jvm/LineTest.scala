
package scala.tools.nsc.backend.jvm

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class LineTest {
  import java.lang.{ StackTraceElement => JSTE }
  object StackTraceElement {
    def unapply(ste: JSTE): Option[(String, String, String, Int)] =
      Some((ste.getClassName, ste.getMethodName, ste.getFileName, ste.getLineNumber))
  }

  // assert line in SampleMain stack is not start of template
  @Test def t7594() {
    try SampleMain.main(null)
    catch {
      case t: Throwable => assert {
        t.getStackTrace.toList.sliding(2).exists {
          case StackTraceElement(template, method, file, line) ::
               StackTraceElement(template2, method2, file2, line2) :: Nil =>
            template.contains("Averrable") && method.contains("yes") && (line2 > line)
        }
      }
    }
  }
}

object SampleMain extends App {
  implicit class Averrable(val b: Boolean) extends AnyVal {
    def yes = assert(b)
    def no = assert(!b)
  }

  false.yes
}
