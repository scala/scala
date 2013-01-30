object Test extends App {
  test.Test.test
}

package test {
  object Test {
    def test = {
      Macros.foo
    }
  }
}