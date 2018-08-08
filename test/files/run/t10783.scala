package com.example {
  object X {
    def bar: Int = (new Value(42)).foo
    def baz: Int = (new Walue(42)).foo
    def bip: Int = (new Xalue(42)).foo
  }
}

package com.example {
  class Value(val value: Int) extends AnyVal {
    def foo: Int = value + 1
  }
  object Walue
  class Walue(val value: Int) extends AnyVal {
    def foo: Int = value + 1
  }
  class Xalue(val value: Int) extends AnyVal {
    def foo: Int = value + 1
  }
  object Xalue
}

object Test {
  import com.example._

  def main(args: Array[String]): Unit = {
    assert(X.bar == 43)
    assert(X.baz == 43)
    assert(X.bip == 43)
  }
}