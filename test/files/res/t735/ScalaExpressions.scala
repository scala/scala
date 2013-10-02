package t735;
trait ScalaExpressions {
  trait ExpressionFactory {
    def foo = 10;
    def bar : Int;
  }
  val values : ValueFactory;
  trait ValueFactory extends ExpressionFactory {
    def bar = 42;
  }
}
