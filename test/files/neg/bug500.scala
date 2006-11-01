object Magic {
  abstract class O[X,Y] {
    type T >: X <: Y;
    class I { def magic(v: T): T = v; }
  }
  def magic[X,Y](v: X): Y = {
    val o: O[X,Y] = null;
    val i: o.I = new o.I();
    i.magic(v);
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    try {
      val i: Int = Magic.magic("42");
      Console.println(i);
    } catch {
      case ex: Throwable => ex.printStackTrace()
    }
  }
}
