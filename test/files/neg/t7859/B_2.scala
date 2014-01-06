class C(private val x: Any) extends AnyVal

// Checking that makeNotPrivate(paramAccessor) doesn't make this visible during typer.
// The output is identical with/without `extends AnyVal`.
object Test {
  new p1.A(x).x
  new B(x).x
  new C(x).x
}
