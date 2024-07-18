//> using options -Werror -Wunused:locals
class C {
  def unusedLocal = {
    val a = 27
    42
  }
}
