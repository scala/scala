
import scala.tools.partest.DirectTest
import scala.util.Properties.isWin

object Test extends DirectTest {
  import java.nio.file.Files._

  override def code: String = "class C { val c = new p.J_1().f() }"

  override def show(): Unit = {
    val dir = createTempDirectory("t12019")
    val out = createTempDirectory("t12019out")
    try {
      val target = createDirectory(dir.resolve("java.zip"))
      val outdir = testOutput.jfile.toPath
      val pkgdir = outdir.resolve("p")
      val tocopy = walk(pkgdir)
      try {
        tocopy.forEach { p =>
          val partial = outdir.relativize(p)
          val q = target.resolve(partial)
          copy(p, q)
        }
      } finally {
        tocopy.close()
      }
      val compiler = newCompiler(newSettings("-usejavacp" :: "-classpath" :: target.toString :: "-d" :: out.toString :: Nil))
      compileString(compiler)(code)
    } finally {
      if (!isWin) {
        Zapper.remove(dir)
        Zapper.remove(out)
      }
    }
  }
}

object Zapper {
  import java.io.IOException
  import java.nio.file._, Files._, FileVisitResult.{CONTINUE => Continue}
  import java.nio.file.attribute._

  def remove(path: Path): Unit = if (isDirectory(path)) removeRecursively(path) else delete(path)

  private def removeRecursively(path: Path): Unit = walkFileTree(path, new ZappingFileVisitor)

  private class ZappingFileVisitor extends SimpleFileVisitor[Path] {
    private def zap(path: Path) = { delete(path) ; Continue }
    override def postVisitDirectory(path: Path, e: IOException): FileVisitResult = if (e != null) throw e else zap(path)
    override def visitFile(path: Path, attrs: BasicFileAttributes): FileVisitResult = zap(path)
  }
}
// was: Error accessing /tmp/t120198214162953467729048/java.zip
