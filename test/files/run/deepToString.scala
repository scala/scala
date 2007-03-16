object Test extends Application {

  def sweep(s: String) =
    s.replaceAll("D@[0-9a-fA-F]+", "D@0000000")
     .replaceAll("Z@[0-9a-fA-F]+", "Z@0000000")
     .replaceAll(";@[0-9a-fA-F]+", ";@0000000")

  def test[A](a: Array[A]) {
    Console.println(sweep(a.toString))
    Console.println(a.deepToString)
    Console.println(a.deepMkString("[", ";", "]"))
    Console.println(a.deepMkString(";"))
    Console.println
  }

  val ba1 = Array(true, false)
  val ba2 = Array(ba1, ba1)
  val ba3 = Array(ba2, ba2)
  test(ba1)
  test(ba2)
  test(ba3)

  val da1 = Array(1.0d, 0.0d)
  val da2 = Array(da1, da1)
  val da3 = Array(da2, da2)
  test(da1)
  test(da2)
  test(da3)

  val sa1 = Array("a", "b")
  val sa2 = Array(sa1, sa1)
  val sa3 = Array(sa2, sa2)
  test(sa1)
  test(sa2)
  test(sa3)

  Console.println(Array(Array(true, false), Array(false)).deepMkString("[", "; ", "]"))
  Console.println(Array(Array('1', '2'), Array('3')).deepMkString("[", "; ", "]"))
  Console.println(Array(Array(1, 2), Array(3)).deepMkString("[", "; ", "]"))
  Console.println
}
