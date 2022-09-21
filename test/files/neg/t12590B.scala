// scalac: -Werror -Wunused:locals
object Example extends App {
  def unusedLocal = {
    val a = 1
    2+1
  }
}