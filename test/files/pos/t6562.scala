class Test {

  @inline
  def foo {
    def it = new {}
    (_: Any) => it
  }

  @inline
  private def bar {
    def it = new {}
    (_: Any) => it
  }
}
