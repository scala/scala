//############################################################################
// Bug 281

class Bug281A extends java.util.Hashtable {
    class B { def f = rehash() };
}

object Test {
  def main(args: Array[String]): Unit = {
    val a = new Bug281A;
    val b = new a.B;
    b.f
  }
}
