trait C[A]
object Test {
  def ImplicitParamCA[CC[A], A](implicit ev: C[A]) {implicitly[C[A]]} // must use this exact syntax...
  // error: could not find implicit value for parameter e: C[A]
}
// [[syntax trees at end of typer]]
// abstract trait C#5[A#9116 >: Nothing#5832 <: Any#52] extends scala#33.AnyRef#2780;
// final object Test#15 extends java.lang.Object#2485 with ScalaObject#1913 {
//   def ImplicitParamCA#9123[CC#9124[A#10858 >: Nothing#5832 <: Any#52] >: [A#10858]Nothing#5832 <: [A#10858]Any#52,
//                            A#9125 >: Nothing#5832 <: Any#52](implicit ev#10856: C#5[A#9127]): Unit#3818
//         = scala#34.this.Predef#1683.implicitly#8816[C#5[A#10858]]()
// }
