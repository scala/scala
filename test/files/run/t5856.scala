object Test extends App {
  override def toString = "Test"

  assert(s"$this" == "Test")
  assert(s"$this$this" == "TestTest")
  assert(s"$this$$" == "Test$")
  assert(s"$this.##" == "Test.##")
  assert(s"$this.toString" == "Test.toString")
  assert(s"$this=THIS" == "Test=THIS")
}