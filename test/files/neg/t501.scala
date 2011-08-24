object Magic {
  class O[X,Y] {
    abstract class I { type T >: X <: Y; }
    val i: I = null;
    def magic(v: i.T): i.T = v;
  }
  def magic[X,Y](v: X): Y = {
    val o: O[X,Y] = new O();
    o.magic(v);
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val i: Int = Magic.magic("42");
    Console.println(i);
  }
}
