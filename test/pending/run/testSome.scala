import testing.SUnit._

//trait Foo {
//  def bar:Option[Seq[xml.Node]] = Some(Nil)
//}
object testSome extends Assert {
  //val x: Option[String] = Some("foo")

  //val x1: Option[Int] = Some(3)

  //val y: Option[String] = None

  //val y1: Option[Int] = None

  def main(args:Array[String]) = {

    //assertFalse("some[string].isEmpty ", x.isEmpty) // x eq null
    //assertFalse("some[int].isEmpty ", x1.isEmpty)

  //  assertTrue("some[string].isInstanceOf[Some] ", x.isInstanceOf[Some[String]])
  //  assertTrue("some[int].isInstanceOf[Some] ",    x1.isInstanceOf[Some[Int]])

    //assertTrue("some[string].asInstanceOf[Some] ", {x.asInstanceOf[Some[String]];true})
    //assertTrue("some[int].asInstanceOf[Some] ",    {x1.asInstanceOf[Some[Int]]; true})
    /*
    assertTrue("none<:opt[string].isEmpty ", y.isEmpty)
    assertTrue("non<:opt[int].isEmpty ", y1.isEmpty)

    assertEquals("Some(foo).get ", "foo", x.get)

    assertEquals("Some(3).get ", 3, x1.get)
    val f = {x:String => Some(x.length)}

    val len:Option[Int] = x.flatMap(f)
    Console.println("len: (3) "+len)


    val g = {x:String => x.charAt(0) == 'f'}

    Console.println("filter: (foo) "+x.filter(g))
    // to do:
//    assertEquals("equals", len == Some(3))

    // matching
    x match {
      case Some("foo") => true
      case None        => false
    }

    // matching
    x match {
      case Some(z) => z
      case _       => null
    }
    x match {
      case None => "3"
      case Some(z) => "4"
    }
*/

    new collection.mutable.HashMap[Int,Option[String]]().get(1) match {
      case Some(Some(x)) => 1
      case _ => 2
    }
    // unapply
  }
/*
  def foobar(x:Foo) = {x.bar == 42} // nonsense
  val zz : Option[Int] =  try { Some(1/0) } catch { case _ => None}

  trait ZyGo {
    def impolite: Option[String]
  }
  val foo = new ZyGo {
    val impolite = None
  }

  class Fu {
    def bar: Option[String] = Some("foo")
  }
  new Fu().bar

  def isNullable (metadata:java.sql.ResultSetMetaData, index:Int): Option[scala.Boolean] =
    metadata.isNullable(index) match {
      case java.sql.ResultSetMetaData.columnNoNulls => Some(false);
      case java.sql.ResultSetMetaData.columnNullable => Some(true);
      //case java.sql.ResultSetMetaData.columnNoNulls => None; // bq:unreachable code
    }


  def names_get_self : Option[scala.Symbol] = None

  def send: Tuple1[String] = {
    val senderName = new collection.mutable.HashMap[String, scala.Symbol].get("foo") match {
      case None =>
        "foo"
      case Some(name) =>
        "bar"
    }
    Tuple1(senderName)
  }
*/
}
