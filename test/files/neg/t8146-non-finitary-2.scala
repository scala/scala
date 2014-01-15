// Example 3 from "On Decidability of Nominal Subtyping with Variance" (Pierce, Kennedy)
// http://research.microsoft.com/pubs/64041/fool2007.pdf
trait N[-Z]
trait D[Y]
trait C[X] extends N[N[C[D[X]]]]
object Test {
  def foo(c: C[Int]): N[C[Int]] = c
}
