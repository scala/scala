//############################################################################
// Test Java interaction
//############################################################################
// $Id$

import java.awt.Color;
import java.awt.Point;

class ColoredPoint(x: Int, y: Int, c_ : Color) extends Point(x, y) {
  val c: Color = c_;
  def getC(): Color = c;
}

object Test {
  def main(args: Array[String]): Unit = {
    val p = new ColoredPoint(5, 7, Color.RED);
    System.out.println("p.x = " + p.x);
    System.out.println("p.c = " + p.c);
    System.out.println("p.getX() = " + p.getX());
    System.out.println("p.getC() = " + p.getC());
  }
}

//############################################################################
