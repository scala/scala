class Value {
  def coerce: Int = 0;
}

object Test {

  def foo(i: Int): Int = 0;

  def fun0         : Value = null;
  def fun0(i: Int ): Value = null;

  def fun1(i: Int ): Value = null;
  def fun1(l: Long): Value = null;

  foo(fun0           );
  foo(fun1(new Value));

}
