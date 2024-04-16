
//> using options -Werror -Wunused:params

trait T {
  private var x: String = _

  def y: String = {
    if (x eq null) x = "hello, world"
    x
  }
}
