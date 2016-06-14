
/*
Was:
test/files/pos/t9245.scala:5: error: recursive value catchExpr1 needs type
    try {} catch catchExpr1
                 ^

Now:
    def catchExpr1: PartialFunction[Throwable,Any] = scala.this.Predef.???;
    def test: Any = try {
      ()
    } catch {
      case (x$1 @ (_: Throwable)) => {
        <artifact> val catchExpr$1: PartialFunction[Throwable,Any] = Test.this.catchExpr1;
        if (catchExpr$1.isDefinedAt(x$1))
          catchExpr$1.apply(x$1)
        else
          throw x$1
      }
    }
*/
trait Test {
  def catchExpr1: PartialFunction[Throwable, Any] = ???
  def test = {
    try {} catch catchExpr1
  }
}
