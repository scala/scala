class C1
class C2
class A {
  def f(x: Any) = x
  def g(x: C1): String = "A"
  def g(x: C2): String = "B"

  def crash() = f(List[String]() flatMap { x =>
    if (false) List(g(x)) else List[C1]() map g
  })
}
