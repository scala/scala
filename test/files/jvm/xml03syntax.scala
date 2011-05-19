import scala.xml._

object Test {

  private def handle[A](x: Node): A = {
    println(x)
    x.child(0).asInstanceOf[Atom[A]].data
  }

  def main(args: Array[String]) {
    test1()
    test2()
    test3()
  }

  private def test1() {
    val xNull = <hello>{null}</hello> // these used to be Atom(unit), changed to empty children

    println(xNull.child sameElements Nil)

    val x0 = <hello>{}</hello> // these used to be Atom(unit), changed to empty children
    val x00 = <hello>{ }</hello> //  dto.

    val xa = <hello>{ "world" }</hello>


    println(x0.child sameElements Nil)
    println(x00.child sameElements Nil)
    println(handle[String](xa) == "world")

    val xb = <hello>{ 1.5 }</hello>

    println(handle[Double](xb) == 1.5)

    val xc = <hello>{ 5 }</hello>

    println(handle[Int](xc) == 5)

    val xd = <hello>{ true }</hello>

    println(handle[Boolean](xd) == true)

    val xe = <hello>{ 5:Short }</hello>

    println(handle[Short](xe) == (5:Short))

    val xf = <hello>{ val x = 27; x }</hello>

    println(handle[Int](xf) == 27)

    val xg = <hello>{ List(1,2,3,4) }</hello>

    println(xg)
    for (z <- xg.child) {
      println(z.toString() + {if (z.isInstanceOf[Text]) "(is text node ' ')" else ""})
    }

    val xh = <hello>{ for(x <- List(1,2,3,4) if x % 2 == 0) yield x }</hello>

    println(xh)
    for (z <- xh.child) {
      println(z.toString() + {if (z.isInstanceOf[Text]) "(is text node ' ')" else ""})
    }
    println
  }

  /** see SVN r13821 (emir): support for <elem key={x:Option[Seq[Node]]} />,
   *  so that Options can be used for optional attributes.
   */
  private def test2() {
    val x1: Option[Seq[Node]] = Some(<b>hello</b>)
    val n1 = <elem key={x1} />;
    println("node="+n1+", key="+n1.attribute("key"))

    val x2: Option[Seq[Node]] = None
    val n2 = <elem key={x2} />;
    println("node="+n2+", key="+n2.attribute("key"))
  }

  private def test3() {
    // this demonstrates how to handle entities
    val s = io.Source.fromString("<a>&nbsp;</a>")
    object parser extends xml.parsing.ConstructingParser(s, false /*ignore ws*/) {
      override def replacementText(entityName: String): io.Source = {
        entityName match {
          case "nbsp" => io.Source.fromString("\u0160");
          case _ => super.replacementText(entityName);
        }
      }
      nextch; // !!important, to initialize the parser
    }
    val parsed = parser.element(TopScope) // parse the source as element
    // alternatively, we could call document()
    parsed
  }

}
