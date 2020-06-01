
import scala.tools.partest.DirectTest

import scala.jdk.CollectionConverters._
import scala.tools.testkit.ReleasablePath._
import scala.util.Using

import java.nio.file.Files._

object Test extends DirectTest {

  override def code: String = "class C { val c = new p.J_1().f() }"

  override def show(): Unit =
    Using.resource(createTempDirectory("t12019")) { dir =>
      val target = createDirectory(dir.resolve("java.zip"))
      val outdir = testOutput.jfile.toPath
      val pkgdir = outdir.resolve("p")
      Using.resource(walk(pkgdir)) { str =>
        str.forEach { p =>
          val partial = outdir.relativize(p)
          val q = target.resolve(partial)
          copy(p, q)
        }
      }
      Using.resource(createTempDirectory("t12019out")) { out =>
        val compiler = newCompiler(newSettings("-usejavacp" :: "-classpath" :: target.toString :: "-d" :: out.toString :: Nil))
        compileString(compiler)(code)
      }
    }
}
// was: Error accessing /tmp/t120198214162953467729048/java.zip
