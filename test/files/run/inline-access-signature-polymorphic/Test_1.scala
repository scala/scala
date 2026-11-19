//> using options -opt:inline:** -Yopt-inline-heuristics:everything

// The invokeExact call in p.C.call has the descriptor ()Lp/Pkg;, where p.Pkg is package-private.
// The descriptor is resolved as a method type in the calling class, so the method cannot be
// inlined into Test.
object Test {
  def t = p.C.call(p.C.handle())
  def main(args: Array[String]): Unit = println(t)
}
