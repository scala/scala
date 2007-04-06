// tests transformation of return type in typedTypeApply (see also tcpoly_gm.scala)
class As {
  class A {
    def foo: A.this.type = bar.asInstanceOf[A.this.type]
    def foo2: this.type = bar.asInstanceOf[this.type]
    def bar: A = null
  }
}
