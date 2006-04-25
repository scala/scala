object ShapeTest extends Application {

    class Point(x : int, y : int) {
        override def toString() = "[" + x + "," + y + "]"
    }

    abstract class Shape {
        def draw() : unit
    }

    class Line(s : Point, e : Point) extends Shape {
        def draw() : unit = { Console.println("draw line " + s + "," + e) }
    }

    abstract class Foo {
        type T <: Object

        def show(o : T) : unit
        def print() : unit = {Console.println("in Foo")}
    }

    abstract class ShapeFoo extends Foo {
        type T <: Shape
        def show(o : T) : unit = { o.draw() }
        override def print() : unit = {Console.println("in ShapeFoo")}
    }

    class LineFoo extends ShapeFoo {
        type T = Line
        override def print() : unit = {Console.println("in LineFoo")}
    }

    val p1 = new Point(1,4)
    val p2 = new Point(12, 28)

    val l1 = new Line(p1, p2)


    val l = new ShapeFoo{  // ** //
      type T = Line  // ** //
      override def print() : unit = {Console.println("in LineFoo")} // ** //
    }
    l.show(l1) // ** //
}
