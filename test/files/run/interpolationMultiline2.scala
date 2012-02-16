object Test extends App {

  def test1(n: Int) = {
    val old = "old"
    try { println(s"""Bob is ${s"$n"} years ${s"$old"}!""") } catch { case ex => println(ex) }
    try { println(s"""Bob is ${f"$n"} years ${s"$old"}!""") } catch { case ex => println(ex) }
    try { println(f"""Bob is ${s"$n"} years ${s"$old"}!""") } catch { case ex => println(ex) }
    try { println(f"""Bob is ${f"$n"} years ${s"$old"}!""") } catch { case ex => println(ex) }
    try { println(f"""Bob is ${f"$n%2d"} years ${s"$old"}!""") } catch { case ex => println(ex) }
    try { println(f"""Bob is ${s"$n%2d"} years ${s"$old"}!""") } catch { case ex => println(ex) }
    try { println(s"""Bob is ${f"$n%2d"} years ${s"$old"}!""") } catch { case ex => println(ex) }
    try { println(s"""Bob is ${s"$n%2d"} years ${s"$old"}!""") } catch { case ex => println(ex) }
  }

  test1(1)
  println("===============")
  test1(12)
  println("===============")
  test1(123)

}
