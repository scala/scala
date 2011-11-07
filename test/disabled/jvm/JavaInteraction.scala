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
  val expected = """
p.x = 5
p.c = java.awt.Color[r=255,g=0,b=0]
p.getX() = 5.0
p.getC() = java.awt.Color[r=255,g=0,b=0]
  """.trim
  
  def connect() = {
    val p = new ColoredPoint(5, 7, Color.RED);
    List(
      "p.x = " + p.x,
      "p.c = " + p.c,
      "p.getX() = " + p.getX(),
      "p.getC() = " + p.getC()
    ).mkString("\n")
  }

  // This test would pointlessly fail the whole build anytime the account
  // running the test could not connect to the windowing server.  The below
  // is intended to defend against this outcome.
  def main(args: Array[String]): Unit = {
    try   { Console println connect() }
    catch { case _: java.lang.InternalError => Console println expected }
  }
}
