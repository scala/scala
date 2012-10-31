import scala.reflect.macros.Context

object Macros {
  def impl(c: Context): c.Expr[Unit] = {
    // we're macros, so we can reflect against our source path
    // so we don't need any partests to clean up after us!
    val dir = c.enclosingUnit.source.file.file.getCanonicalFile.getParentFile
    def normalizePaths(s: String) = {
      val base = (dir.getCanonicalPath + java.io.File.separator).replace('\\', '/')
      var regex = """\Q%s\E""" format base
      val isWin = System.getProperty("os.name", "") startsWith "Windows"
      if (isWin) regex = "(?i)" + regex
      s.replace('\\', '/').replaceAll(regex, "")
    }

    import c.universe._
    val next = if (c.enclosingMacros.length < 3) c.Expr[Unit](Select(Ident(c.mirror.staticObject("Macros")), newTermName("foo"))) else c.literalUnit
    c.universe.reify {
      println(c.literal(normalizePaths(c.enclosingMacros.toString)).splice)
      next.splice
    }
  }

  def foo = macro impl
}