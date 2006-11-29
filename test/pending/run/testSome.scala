import testing.SUnit._

object testSome extends Assert {

  val x: Option[String] = Some("foo")

  val x1: Option[Int] = Some(3)

  val y: Option[String] = None

  val y1: Option[Int] = None

  def main(args:Array[String]) = {
    assertFalse("some[string].isEmpty ", x.isEmpty) // x eq null

    assertFalse("some[int].isEmpty ", x1.isEmpty)

    assertTrue("none<:opt[string].isEmpty ", y.isEmpty)

    assertTrue("non<:opt[int].isEmpty ", y1.isEmpty)

    Console.println(x.get) // x

    Console.println(x1.get)

    val f = {x:String => Some(x.length)}

    val len:Option[Int] = x.flatMap(f)
    Console.println("len: (3) "+len)

    val g = {x:String => x.charAt(0) == 'f'}

    Console.println("filter: (foo) "+x.filter(g))

    // to do:

    //assertEquals("equals", len == Some(3))

    // matching

    // unapply
  }
}
