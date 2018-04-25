class Test {

  @inline
  def foo: Unit = {
    def it = new {}
    (_: Any) => it
  }

  @inline
  private def bar: Unit = {
    def it = new {}
    (_: Any) => it
  }
}
