//> using options -opt:inline:** -Yopt-inline-heuristics:everything

// The lambda in p.C.mk implements the package-private interface p.PkgFI, so the method cannot
// be inlined into Test, even though the lambda's implementation method p.C.pub is public.
object Test {
  def t = p.C.run(p.C.mk())
  def main(args: Array[String]): Unit = println(t)
}
