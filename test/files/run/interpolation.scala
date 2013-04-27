object Test extends App {

  def test1(n: Int) = {
    println(s"Bob is $n years old")
    println(f"Bob is $n%2d years old")
    println(s"Bob will be ${n+1} years old")
    println(f"Bob will be ${n+1}%2d years old")
    println(s"$n+1 = ${n+1}")
    println(f"$n%d+1 = ${n+1}%d")
  }

  def test2(f: Float) = {
    println(s"Best price: $f")
    println(f"Best price: $f%.2f")
    println(s"$f% discount included")
    println(f"$f%3.2f%% discount included")
  }

  test1(1)
  test1(12)
  test1(123)

  test2(10.0f)
  test2(13.345f)

  println(s"")
  println(s"${0}")
  println(s"${0}${0}")
  println(f"")
  println(f"${0}")
  println(f"${0}${0}")
}
