
//> using options -Werror -Xlint

object Strings {
  final val greeting = "hello"
  final val lib = "MyLib"
  final val version = 17
}

class C {
  import Strings._

  @deprecated(s"$greeting world", since=s"$lib $version")
  def s = 42

  @deprecated(f"$greeting world", since=f"$lib $version")
  def f = 42

  def t = s

  def g = f
}
