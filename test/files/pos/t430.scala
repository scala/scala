object Test extends App {
  def foo[T <% Ordered[T]](x: T){ Console.println(""+(x < x)+" "+(x <= x)) }
  def bar(x: Unit   ): Unit = foo(x);
  def bar(x: Boolean): Unit = foo(x);
  def bar(x: Byte   ): Unit = foo(x);
  def bar(x: Short  ): Unit = foo(x);
  def bar(x: Int    ): Unit = foo(x);
  def bar(x: Long   ): Unit = foo(x);
  def bar(x: Float  ): Unit = foo(x);
  def bar(x: Double ): Unit = foo(x);
  bar(())
  bar(true)
  bar(1: Byte)
  bar(1: Short)
  bar('a')
  bar(1)
  bar(1l)
  bar(1.0f)
  bar(1.0)
}
