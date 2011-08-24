trait D {
  private val x = "xxxx should appear twice"
  private object xxxx { Console.println(x) }
  def get_xxxx: AnyRef = xxxx
}

trait E extends D {
  def f(): Unit = {
    val y = "yyyy should appear twice"
    object yyyy {
      val x1 = get_xxxx
      Console.println(y)
    }
    yyyy
  }
}
class C extends E {}
object Go extends D {
  def main(args : Array[String]) {
    new C().f()
    new C().f()
  }
}
