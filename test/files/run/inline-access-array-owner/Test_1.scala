//> using options -opt:inline:** -Yopt-inline-heuristics:everything

// p.Pkg is package-private. The method reference in `copy` names the array class p.Pkg[], so it
// cannot be inlined into Test. There is no CHECKCAST, as the result type is Object.
package p {
  object D {
    def make: Array[Pkg] = new Array[Pkg](1)
    def copy(a: Array[Pkg]): Object = a.clone()
  }
}

object Test {
  def t: Object = p.D.copy(p.D.make)
  def main(args: Array[String]): Unit = println(t.asInstanceOf[Array[_]].length)
}
