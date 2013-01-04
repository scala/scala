object Test extends App {
  def f(x: Boolean) = if (x) Array("abc") else Array()
  try {
    println(f(true).length)
    println(f(false).length)
  } catch {
    case ex: Throwable => println(ex.getMessage)
  }
}
