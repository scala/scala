//############################################################################
// Bugs
//############################################################################
// $Id$

//############################################################################
// Bug 135

object Bug135Test {

  import scala.collection.immutable.TreeMap;
  import scala.collection.immutable.Order;

  def main(args: Array[String]): Unit = {
    val intOrder =
	new Order((x:int,y:int) => x < y, (x:int,y:int) => x == y);
    val myMap:TreeMap[int,String] = new TreeMap(intOrder);
    val map1 = myMap + 42 -> "The answer";
    if (map1.get(42) != Some("The answer"))
      Console.println("KO: " + map1.get(42));
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
// Bug 168

class Bug168Foo {
  class Bar;
  def foo = new Bar;
}

object Bug168Test  {
  def main(args: Array[String]): Unit = {
    (new Bug168Foo).foo;
    ()
  }
}

//############################################################################
// Main

object Test  {
  def main(args: Array[String]): Unit = {
    Bug135Test.main(args);
    Bug167Test.main(args);
    Bug168Test.main(args);
  }
}

//############################################################################
