object Test {
  def e(msg: String) = new Exception(msg)

  // this code ain't dead.
  def a(b: Boolean) = {
    b match {
      case true => throw e("true")
      case false => throw e("false")
    }
  }
}
