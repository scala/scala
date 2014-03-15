class C {
  def foo: Int = 0
}

class E extends C {
  override def foo: Int = {
    (None: Option[Int]).getOrElse {
      class C
      E.super.foo 
    }
  }
}

object Test {
  def main(args: Array[String]) {
    new E().foo
  }
}
