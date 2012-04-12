import scala.reflect.makro.Context

object Macros {
  def impl(c: Context): c.Expr[Unit] = {
    // we're macros, so we can reflect against our source path
    // so we don't need any partests to clean up after us!
    val c.CompilationUnit(file, _, _) = c.enclosingUnit
    val dir = file.getParentFile
    def normalizePaths(s: String) = {
      val base = (dir.getCanonicalPath + java.io.File.separator).replace('\\', '/')
      var regex = """\Q%s\E""" format base
      val isWin = System.getProperty("os.name", "") startsWith "Windows"
      if (isWin) regex = "(?i)" + regex
      s.replace('\\', '/').replaceAll(regex, "")
    }
    
    import c.mirror._
    val next = if (c.enclosingMacros.length < 3) Expr[Unit](Select(Ident(staticModule("Macros")), newTermName("foo"))) else c.literalUnit
    c.reify {
      println(c.literal(normalizePaths(c.enclosingMacros.toString)).eval)
      next.eval
    }
  }

  def foo = macro impl
}