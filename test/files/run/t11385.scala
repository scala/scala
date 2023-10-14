
import scala.tools.partest.DirectTest

import java.nio.file.Files.{createDirectories, createTempDirectory}
import scala.tools.testkit.ReleasablePath._
import scala.util.Using

// an unfortunately-named resource dir on the classpath
//
object Test extends DirectTest {

  def code = "package acme { class C }"

  def show() = assert {
    Using.resource(createTempDirectory("t11385")) { tmp =>
      val pkg = createDirectories(tmp.resolve("acme").resolve("C").resolve("sub"))
      compile("-classpath", tmp.toString)
    }
  }
}

/* Was:
error: Error while emitting newSource1.scala
assertion failed:
  Java member module without member class: package sub - List(package sub)
     while compiling: newSource1.scala
        during phase: jvm
     library version: version 2.13.2-20191220-000408-7bfe4c0
    compiler version: version 2.13.2-20191220-000408-7bfe4c0
  reconstructed args: -usejavacp -classpath /var/folders/2_/xb149z895wb5f1y632xp2bw40000gq/T/t113851788670762590621498 -d t11385-run.obj
 */
