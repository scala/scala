//> using options -opt:inline:** -Yopt-inline-heuristics:everything

// The exception handler in p.C.tryIt catches the package-private p.PkgEx, so the method
// cannot be inlined into Test.
object Test {
  def t = p.C.tryIt(() => { p.C.thrower(); 1 })
  def main(args: Array[String]): Unit = println(t)
}
