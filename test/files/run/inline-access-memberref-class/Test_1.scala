//> using options -opt:inline:** -Yopt-inline-heuristics:everything

// p.B is package-private. The field and method references in p.C name p.B, so they cannot be
// inlined into Test, even though the members are declared in the public class p.A.
object Test {
  def main(args: Array[String]): Unit = {
    println(p.C.field())
    println(p.C.method())
  }
}
