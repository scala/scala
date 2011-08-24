class Value {}

object Test {

  implicit def view(v: Value): Int = 0

  def foo(i: Int): Int = 0

  def fun0         : Value = null
  def fun0(i: Int ): Value = null

  def fun1(i: Int ): Value = null
  def fun1(l: Long): Value = null

  foo(fun0           );
  foo(fun1(new Value));

}
