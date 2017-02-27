package xsbt

abstract class Compat
object Compat

private trait CachedCompilerCompat { self: CachedCompiler0 =>
  def newCompiler: Compiler = new Compiler()
}
