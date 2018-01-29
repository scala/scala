trait T {
  private var s: String = _
  def getS: String = {
    if (s == null) {
      s = ""
    }
    s
  }
}

class C {
  private var f: String = _
  def getF: String = {
    if (f == null) {
      f = ""
    }
    f
  }
}

object Test extends C with T {
  def main(args: Array[String]): Unit = {
    assert(getS == "")
    assert(getF == "")
  }
}
