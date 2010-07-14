object Test {
  type valueType = { def value: this.type }

  class StringValue(x: String) {
    def value: this.type = this
  }

  def m(x: valueType) = x.value

  val s = new StringValue("hei")

  def main(args: Array[String]) {
    m(s)
  }
}
