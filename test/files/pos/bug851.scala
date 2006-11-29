object test {
  def ok1: String = {
    val x = 1
    """ok1"""
  }
  def ok2: String = {
    val x = "0".length
    """ok2"""
  }
  def ok3: String = {
    val x = "0".length
    """ok3
    """//
  }
  def ok4: String = {
    val x = "0".length
    """ok4
    """ + x
  }
  def bad: String = {
    val x = "0".length
    """bad
    """
  }
  def main(args: Array[String]) {
    Console.println(ok1 + ok2 + ok3 + ok4 + bad)
  }
}
