object Test {

  import scala.testing.UnitTest._
  import scala.xml._

  def handle[A](x:Node): A = {
    Console println x
    x.child(0).asInstanceOf[Atom[A]].data
  }

  def main(args:Array[String]) = {
    import scala.xml.NodeSeq
    import NodeSeq.view
    import testing.UnitTest._

    val x0 = <hello>{}</hello>
    val x00 = <hello>{ }</hello>
    val xa = <hello>{ "world" }</hello>


    assertEquals( handle[Unit](x0),  {} )
    assertEquals( handle[Unit](x00), {} )
    assertEquals( handle[String](xa),"world" )

    val xb = <hello>{ 1.5 }</hello>

    assertEquals( handle[Double](xb), 1.5 )

    val xc = <hello>{ 5 }</hello>

    assertEquals( handle[Int](xc), 5 )

    val xd = <hello>{ true }</hello>

    assertEquals( handle[Boolean](xd), true )

    val xe = <hello>{ 5:short }</hello>

    assertEquals( handle[Short](xe), 5:short )

    val xf = <hello>{ val x = 27; x }</hello>

    assertEquals( handle[Int](xf), 27 )

    val xg = <hello>{ List(1,2,3,4) }</hello>

    Console println xg
    for(val z <- xg.child) {
      Console println z.toString() + {if (z.isInstanceOf[Text]) "(is text node ' ')" else ""}
    }

    val xh = <hello>{ for(val x <- List(1,2,3,4); x % 2 == 0) yield x }</hello>

    Console println xh
    for(val z <- xh.child) {
      Console println z.toString() + {if (z.isInstanceOf[Text]) "(is text node ' ')" else ""}
    }


}

}
