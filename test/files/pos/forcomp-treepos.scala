//> using options -Yvalidate-pos:typer
object A {
  def foo(list: List[String]) = for (string <- list if string.length > 5)
    println(string)
}
