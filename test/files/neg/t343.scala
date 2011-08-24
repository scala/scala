package scalaInner1;

class C {
  private class Foo {}
  def get:Foo = new Foo();
}



object Test  {
  def main(args:Array[String]) = {
    val c = new C().get;
  }
}
