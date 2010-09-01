object Test extends Application {
  def test(f: () => Int) = {
    val x = f()
    5
  }

  println(test(() => { println("hi there"); 0 }))
}
