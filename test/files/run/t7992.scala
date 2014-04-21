class C {
  def foo: Int = 0
}

class D extends C {
  override def foo: Int = {
    val f = () => {
      class C     // comment this line to fix.
      D.super.foo // no super accessor generated here!
      // java.lang.VerifyError: (class: D$$anonfun$1, method: apply$mcI$sp signature: ()I) Illegal use of nonvirtual function call
    }
    f()
  }
}

object Test {
  def main(args: Array[String]) {
    new D().foo
  }
}
