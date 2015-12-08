class Foo_1 {
  def foo() {
	// constant optimization should eliminate all branches
    val i = 1
    val x = if (i != 1) null else "good"
    val y = if (x == null) "good" else x + ""
    println(y)
  }
}