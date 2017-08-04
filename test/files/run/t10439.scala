object Test {
  private var s: String = _

  def getS: String = {
    if (s == null) {
      s = ""
    }
    s
  }

  def main(args: Array[String]): Unit = {
    assert(getS == "")
  }
}
