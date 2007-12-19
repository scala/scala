import scala.testing.SUnit._
import scala.xml._

object Test extends AnyRef with Assert {

  private def handle[A](x: Node): A = {
    println(x)
    x.child(0).asInstanceOf[Atom[A]].data
  }

  def main(args: Array[String]) {

    val xNull = <hello>{null}</hello> // these used to be Atom(unit), changed to empty children

    assertSameElements(xNull.child, Nil)

    val x0 = <hello>{}</hello> // these used to be Atom(unit), changed to empty children
    val x00 = <hello>{ }</hello> //  dto.

    val xa = <hello>{ "world" }</hello>


    assertSameElements(x0.child,  Nil)
    assertSameElements(x00.child, Nil)
    assertEquals(handle[String](xa), "world")

    val xb = <hello>{ 1.5 }</hello>

    assertEquals(handle[Double](xb), 1.5)

    val xc = <hello>{ 5 }</hello>

    assertEquals(handle[Int](xc), 5)

    val xd = <hello>{ true }</hello>

    assertEquals(handle[Boolean](xd), true)

    val xe = <hello>{ 5:short }</hello>

    assertEquals(handle[Short](xe), 5:short)

    val xf = <hello>{ val x = 27; x }</hello>

    assertEquals(handle[Int](xf), 27)

    val xg = <hello>{ List(1,2,3,4) }</hello>

    println(xg)
    for (z <- xg.child) {
      println(z.toString() + {if (z.isInstanceOf[Text]) "(is text node ' ')" else ""})
    }

    val xh = <hello>{ for(val x <- List(1,2,3,4); x % 2 == 0) yield x }</hello>

    println(xh)
    for (z <- xh.child) {
      println(z.toString() + {if (z.isInstanceOf[Text]) "(is text node ' ')" else ""})
    }

  }

}
