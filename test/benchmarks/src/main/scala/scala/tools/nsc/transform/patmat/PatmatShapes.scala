package scala.tools.nsc.transform.patmat

/** Generated matches of `n` cases, one per shape that the pattern matching analysis
 *  used to handle in time quadratic (or worse) in `n`.
 *
 *  The sources are plain Scala that also compile with Scala 3, so the same shapes can be
 *  used to compare the two compilers.
 *
 *  A shape name ending in `R` gives the case classes a second field of the sealed type itself
 *  (`C_i(a: Int, b: T)`), which makes the analysis of `T`'s domain recursive.
 */
object PatmatShapes {
  val shapes: List[String] = List("ctor", "typ", "tup", "opt", "guard", "seq", "ints")
  val all: List[String]    = shapes ++ List("ctorR", "tupR", "optR")

  def source(shape: String, n: Int): String = {
    val rec  = shape.endsWith("R")
    val base = if (rec) shape.init else shape
    val sb   = new java.lang.StringBuilder
    def cases(f: Int => String): Unit = (0 until n).foreach(i => sb.append("  case ").append(f(i)).append(" => ").append(i).append('\n'))
    base match {
      case "ctor"  => sb.append("def f(t: T): Int = t match {\n"); cases(i => s"C$i(_, _)")
      case "typ"   => sb.append("def f(t: T): Int = t match {\n"); cases(i => s"_: C$i")
      case "tup"   => sb.append("def f(a: T, b: T): Int = (a, b) match {\n"); cases(i => s"(C$i(_, _), C$i(_, _))"); sb.append("  case _ => -1\n")
      case "opt"   => sb.append("def f(o: Option[T]): Int = o match {\n"); cases(i => s"Some(C$i(_, _))"); sb.append("  case None => -1\n")
      case "guard" => sb.append("def f(t: T, g: Boolean): Int = t match {\n"); cases(i => s"C$i(_, _) if g"); sb.append("  case _ => -1\n")
      case "seq"   => sb.append("def f(s: Vector[T]): Int = s match {\n  case Vector() => -1\n"); cases(i => s"C$i(_, _) +: _"); sb.append("  case _ => -2\n")
      // 2n literals, so that its N axis is comparable to the others in cost per case
      case "ints"  => sb.append("def f(x: Int): Int = x match {\n"); (0 until 2 * n).foreach(i => sb.append("  case ").append(i).append(" => ").append(i).append('\n')); sb.append("  case _ => -1\n")
      case _       => throw new IllegalArgumentException(s"unknown shape $shape, expected one of ${all.mkString(", ")}")
    }
    sb.append("}\n")
    val defs =
      if (base == "ints") ""
      else (0 until n).map(i => s"final case class C$i${if (rec) "(a: Int, b: T)" else "(a: Int, b: Int)"} extends T\n").mkString("sealed trait T\n", "", "")
    defs + "object O {\n" + sb + "}\n"
  }
}
