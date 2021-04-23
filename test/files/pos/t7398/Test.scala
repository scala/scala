class Test extends Iterator[String] {
  def hasNext = true
  def next() = ""
  def test = this.remove()
}
