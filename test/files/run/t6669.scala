import java.io.{ByteArrayOutputStream, PrintStream}
import scala.reflect.io.File

object Test extends App {
  val baos = new ByteArrayOutputStream()
  val ps = new PrintStream(baos)

  // first test with the default classpath
  (scala.Console withOut ps) {
    scala.tools.scalap.Main.main(Array("-verbose", "java.lang.Object"))
  }

  // on java 10, lone . instead of something/.
  //val currentLocationCpFragment = File.pathSeparator + "."

  // let's assume dirs don't normally have dots
  def hasCurrentDir(s: String): Boolean = s.linesIterator.next.split("[ ,:;]").exists(_.endsWith("."))

  // now make sure we saw the '.' in the classpath
  val msg1 = baos.toString()
  assert(hasCurrentDir(msg1), s"Did not see '.' in the default class path. Full results were:\n$msg1")

  // then test again with a user specified classpath
  baos.reset

  (scala.Console withOut ps) {
    scala.tools.scalap.Main.main(Array("-verbose", "-cp", "whatever", "java.lang.Object"))
  }

  // now make sure we did not see the '.' in the classpath
  val msg2 = baos.toString()
  assert(!hasCurrentDir(msg2), s"Did see '.' in the user specified class path. Full results were:\n$msg2")
}
