class Test {

  {
    def foo() = 1;
    def foo() = 2;
    foo()
  }

  def foo() = 1;
  def foo() = 2;
  foo();

  val x: int = 1;
  def x(): int = 2;
  var x: int = 3;

}
