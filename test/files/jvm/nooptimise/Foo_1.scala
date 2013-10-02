class Foo_1 {
  def foo() {
    // optimization will remove this magic 3 from appearing in the source
    // so -Ynooptimize should prevent that
    val x = 3

  }
}
