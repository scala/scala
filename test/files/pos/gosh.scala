object ShapeTest extends App {

  class Point(x: Int, y: Int) {
    override def toString() = "[" + x + "," + y + "]"
  }

  abstract class Shape {
    def draw(): Unit
  }

  class Line(s: Point, e: Point) extends Shape {
    def draw() { Console.println("draw line " + s + "," + e) }
  }

  abstract class Foo {
    type T <: Object

    def show(o: T): Unit
    def print() { Console.println("in Foo") }
  }

  abstract class ShapeFoo extends Foo {
    type T <: Shape
    def show(o: T) { o.draw() }
    override def print() { Console.println("in ShapeFoo") }
  }

  class LineFoo extends ShapeFoo {
    type T = Line
    override def print() { Console.println("in LineFoo") }
  }

  val p1 = new Point(1,4)
  val p2 = new Point(12, 28)

  val l1 = new Line(p1, p2)


  val l = new ShapeFoo {  // ** //
    type T = Line  // ** //
    override def print() { Console.println("in LineFoo") } // ** //
  }
  l.show(l1) // ** //
}
