object Test extends App {
  val qual: Dynamic = ???
  val expr = "expr"
  val a = "a"
  val a2 = "a2"

  qual.sel(a, a2: _*)
  qual.sel(arg = a, a2: _*)
  qual.sel(arg, arg2 = "a2", a2: _*)
}