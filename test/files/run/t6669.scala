import java.io.{ByteArrayOutputStream, PrintStream}

object Test extends App {
  val baos = new ByteArrayOutputStream()
  val ps = new PrintStream(baos) 

  // first test with the default classpath
  (scala.Console withOut ps) {
    scala.tools.scalap.Main.main(Array("-verbose", "java.lang.Object"))
  }

  // now make sure we saw the '.' in the classpath
  val msg1 = baos.toString()
  assert(msg1 contains "directory classpath: .", s"Did not see '.' in the default class path. Full results were:\n$msg1")

  // then test again with a user specified classpath
  baos.reset

  (scala.Console withOut ps) {
    scala.tools.scalap.Main.main(Array("-verbose", "-cp", "whatever", "java.lang.Object"))
  }

  // now make sure we did not see the '.' in the classpath
  val msg2 = baos.toString()
  assert(!(msg2 contains "directory classpath: ."), s"Did saw '.' in the user specified class path. Full results were:\n$msg2")
}
