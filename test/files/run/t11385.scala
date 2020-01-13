
import scala.tools.partest.DirectTest

import java.nio.file.Files.{createDirectories, createTempDirectory}

// an unfortunately-named resource dir on the classpath
//
object Test extends DirectTest {

  def code = "package acme { class C }"

  def show() = assert {
    val tmp = createTempDirectory("t11385")
    val pkg = createDirectories(tmp.resolve("acme").resolve("C").resolve("sub"))
    compile("-usejavacp", "-classpath", tmp.toString)
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
