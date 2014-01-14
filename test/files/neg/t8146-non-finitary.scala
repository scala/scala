// Example 3 from "On Decidability of Nominal Subtyping with Variance" (Pierce, Kennedy)
// http://research.microsoft.com/pubs/64041/fool2007.pdf
trait N[-A]
trait C[A] extends N[N[C[C[A]]]]
object Test {
  def foo(c: C[Int]): N[C[Int]] = c
}
