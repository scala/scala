object Geom {
  trait Shape
  case class Point(x: Int, y: Int) extends Shape
  case class Rectangle(ll: Point, ur: Point) extends Shape {
    def inset(delta: Int) =
      Rectangle(Point(ll.x - delta, ll.y - delta), Point(ur.x + delta, ur.y + delta));
  }
}

object Color {
  type Color = Int
  val black = 0x000000
  val grey  = 0x808080
}

trait Screen {
  type Color = Int
  def drawRect(r: Geom.Rectangle, c: Color): Unit
  def fillRect(r: Geom.Rectangle, c: Color): Unit
}

object DummyScreen extends Screen {
  def drawRect(r: Geom.Rectangle, c: Color) {
    Console.println("draw " + r + " with " + c)
  }
  def fillRect(r: Geom.Rectangle, c: Color) {
    Console.println("fill " + r + " with " + c)
  }
}

object GUI {

  object Controller {
    def addMouseCtl(c: MouseCtl) = ()
  }

  trait Glyph {
    def getRect: Geom.Rectangle
    def setLoc(p: Geom.Point): Unit
    def draw() { Console.println("draw " + this) }
  }

  class Label(scr: Screen, p: Geom.Point, name: String) extends Glyph {
    private var origin = p
    def getRect = Geom.Rectangle(origin, origin).inset(10);
    def setLoc(p: Geom.Point) = { origin = p }
  }

  trait Ctl {
    def getGlyph: Glyph
    def enable(b: Boolean): this.type
  }

  trait MouseCtl extends Ctl {
    def mouseDown(p: Geom.Point): Unit
  }

  abstract class Button(scr: Screen, p: Geom.Point, name: String)
  extends Glyph with MouseCtl {
    var enabled: Boolean = false
    val label = new Label(scr, p, name)

    /* Glyph methods */
    override def draw() {
      if (enabled) scr.drawRect(getRect, Color.black)
      else scr.fillRect(getRect, Color.grey);
      label.draw();
    }
    def setLoc(p: Geom.Point) = label.setLoc(p);
    def getRect = label.getRect.inset(-2);

    /* Ctl methods */
    def enable(b: Boolean): this.type = { enabled = b; draw(); this }
    def getGlyph = label
    final def mouseDown(p: Geom.Point) {
      if (enabled) doit() else Console.println("button is disabled");
    }
    /* deferred method to be specified by client */
    def doit(): Unit
  }
}

object GUIClient {

  class App {
    def quit() { Console.println("application exited") }
  }

  class QuitButton (scr: Screen, p: Geom.Point, name: String, a: App)
  extends GUI.Button(scr, p, name) {
    def doit() { a.quit() }
  }

  def main(args: Array[String]) {
    val b = new QuitButton(
      DummyScreen, Geom.Point(1, 1), "quit", new App);
    b.draw();
    b.enable(true).mouseDown(Geom.Point(1, 2));
  }
}

