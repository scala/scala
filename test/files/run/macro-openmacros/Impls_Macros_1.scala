//> using options -Yrangepos:false
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.util.Properties.isWin

object Macros {
  def impl(c: Context): c.Expr[Unit] = {
    // we're macros, so we can reflect against our source path
    // so we don't need any partests to clean up after us!
    val dir = c.enclosingUnit.source.file.file.getCanonicalFile.getParentFile
    def normalizePaths(s: String) = {
      val base = (dir.getCanonicalPath + java.io.File.separator).replace('\\', '/')
      var regex = """\Q%s\E""" format base
      if (isWin) regex = "(?i)" + regex
      s.replace('\\', '/').replaceAll(regex, "")
    }

    import c.universe._
    val next = if (c.enclosingMacros.length < 3) c.Expr[Unit](Select(Ident(c.mirror.staticModule("Macros")), TermName("foo"))) else c.Expr[Unit](Literal(Constant(())))
    c.universe.reify {
      println(c.Expr[String](Literal(Constant(normalizePaths(c.enclosingMacros.toString)))).splice)
      next.splice
    }
  }

  def foo: Unit = macro impl
}
