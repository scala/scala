object WithOpTest {
  trait WithOp extends Cloneable {
    def f: this.type = this
    def g1: this.type = f
    def g2: this.type = {
      val t = f
      t
    }
  }
}
