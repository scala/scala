//############################################################################
// Test Java interaction
//############################################################################

import java.awt.Color;
import java.awt.Point;

class ColoredPoint(x: Int, y: Int, c_ : Color) extends Point(x, y) {
  val c: Color = c_;
  def getC(): Color = c;
}

object Test {
  def main(args: Array[String]): Unit = {
    val p = new ColoredPoint(5, 7, Color.RED);
    Console.println("p.x = " + p.x);
    Console.println("p.c = " + p.c);
    Console.println("p.getX() = " + p.getX());
    Console.println("p.getC() = " + p.getC());
  }
}

//############################################################################
