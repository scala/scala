//############################################################################
// Bugs
//############################################################################
// $Id$

//############################################################################
// Bug 135

class Bug135Foo {
  class Bar;
  def foo = new Bar;
}

object Bug135Test  {
  def main(args: Array[String]): Unit = {
    (new Bug135Foo).foo;
    ()
  }
}

//############################################################################
// Bug 167

class Bug167Node(bar:Int) {
  val foo = {
    val bar = 1;
    bar
  }
}

object Bug167Test {
  def main(args: Array[String]): Unit = {
    if (new Bug167Node(0).foo != 1) System.out.println("bug 167");
  }
}

//############################################################################
// Main

object Test  {
  def main(args: Array[String]): Unit = {
    Bug135Test.main(args);
    Bug167Test.main(args);
  }
}

//############################################################################
