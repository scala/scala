//############################################################################
// Programmation IV - 2002 - Week 06
//############################################################################

/** Two-dimensional vector. */
class Vector (_x: Double, _y: Double) {
  def x: Double = _x;
  def y: Double = _y;
  def +(that: Vector): Vector = new Vector(x + that.x, y + that.y);
  def *(scalar: Double): Vector = new Vector(x * scalar, y * scalar);
  def -(that: Vector): Vector = new Vector(x - that.x, y - that.y);
  def /(scalar: Double): Vector = new Vector(x / scalar, y / scalar);
  def norm: Double = Math.sqrt(x * x + y * y);
}

//############################################################################

/** Frame. */
class Frame (_origin: Vector, _edgeX: Vector, _edgeY: Vector) {
  def origin: Vector = _origin;
  def edgeX: Vector = _edgeX;
  def edgeY: Vector = _edgeY;
  /** The vector v in the absolute (drawing) coordinate system */
  def coordMap(v: Vector): Vector = origin + (edgeX * v.x) + (edgeY * v.y);
}

//############################################################################

/** Space on which we can draw lines. */
abstract class Graphics(_width: Double, _height: Double) {
  /** Width of the picture.*/
  def width: Double = _width;

  /** Height of the picture.*/
  def height: Double = _height;

  /** Frame that represents the drawable area of the output device*/
  val frame: Frame;

  /** Draw a line in device coordinates*/
  def plotLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit;

  /** Draw a line in logical coordinates*/
  def drawLine(v1: Vector, v2: Vector): Unit = {
    val _v1 = frame.coordMap(v1);
    val _v2 = frame.coordMap(v2);
    plotLine(_v1.x, _v1.y, _v2.x, _v2.y);
  }

  /** Draw a segment of the picture.*/
  def drawSegment(frm: Frame)(v1: Vector, v2: Vector): Unit = {
    val _v1 = frm.coordMap(v1);
    val _v2 = frm.coordMap(v2);
    drawLine(_v1, _v2);
  }

  /** Draw a list of segments on the picture.*/
  def drawSegments(frm: Frame)(segments: List[Tuple2[Vector, Vector]]): Unit =
    if (segments.isEmpty) ()
    else {
      drawSegment(frm)(segments.head._1, segments.head._2);
      drawSegments(frm)(segments.tail)
    }

  /** Draw a list of continuous segments on the picture.*/
  def drawPolySegment(frm: Frame)(points: List[Vector]) : Unit =
    if (!points.tail.isEmpty) {
      drawSegment(frm)(points.head, points.tail.head);
      drawPolySegment(frm)(points.tail);
    }

  /** updates the contents of the output device*/
  def repaint = ();

  /** Add the last touch to the picture.*/
  def close : Unit;
}

//############################################################################

/** Provides PostScript output. The name of the file is the first parameter
 *  of the constructor. The width and height determine the aspect ratio
 */
class PostScript (filename: String, _width: Double, _height: Double)
 extends Graphics(_width, _height) {
  /** Convert mm into 72th of inch.*/
  def mm2ps(x: Double) : Double = round(x * 72.0 / 25.4);

   def round(x: Double): Double =
     Math.floor(x * 100.0 + 0.5) / 100.0;

  def scaleAndCenter(frm: Frame, ratio:Double): Frame = {
    val currentRatio = frm.edgeX.norm / frm.edgeY.norm;
    if (currentRatio < ratio) {
      val newEdgeX = frm.edgeX;
      val newEdgeY = frm.edgeY * (currentRatio /ratio);
      val newOrigin = frm.origin + ((frm.edgeY - newEdgeY) / 2);
      new Frame(newOrigin, newEdgeX, newEdgeY)
    }
    else {
      val newEdgeX = frm.edgeX * (ratio / currentRatio);
      val newEdgeY = frm.edgeY;
      val newOrigin = frm.origin + ((frm.edgeX - newEdgeX) / 2);
      new Frame(newOrigin, newEdgeX, newEdgeY)
    }
  }

  /** Line thickness in millimeters.*/
  val line_thickness : Double = 0.05;

  /** Width, height, left and right margins in mm.*/
  val psWidth: Double = 210.0;
  val psHeight: Double = 297.0;
  val psWidthMargin: Double = 15.0;
  val psHeightMargin: Double = 15.0;

  val frame: Frame = {
    val origin = new Vector(mm2ps(psWidthMargin), mm2ps(psHeightMargin));
    val edgeX = new Vector(mm2ps(psWidth) - 2 * mm2ps(psWidthMargin), 0);
    val edgeY = new Vector(0, mm2ps(psHeight) - 2 * mm2ps(psHeightMargin));
    scaleAndCenter(new Frame(origin, edgeX, edgeY), width / height)
  }

  def plotLine(x1: Double, y1: Double, x2: Double, y2: Double): Unit = {
    Console.println(round(x1) + " " + round(y1) + " m " +
                    round(x2) + " " + round(y2) + " l");
  }

  /** Print the PS header.*/
  Console.println("%!PS-Adobe-3.0 EPSF-3.0\n%%Title: ProgrammationIV");
  Console.println("%%Creator: LAMP");
  Console.println("%%BoundingBox: 0 0 " + mm2ps(psWidth) + " " + mm2ps(psHeight));
  Console.println("%%EndComments\n");
  Console.println("/m {moveto} bind def\n/l {lineto} bind def\n");
  Console.println(mm2ps(line_thickness) + " setlinewidth\nnewpath");

  /** Terminate the PS document and close the file stream. */
  def close : Unit = {
    Console.println("stroke\nshowpage\n%%EOF");
    Console.flush;
  }
}

//############################################################################

object M0 {

  /** Define the type of a painter as a function that takes a frame,
   *  draws itself onto it and returns nothing
   */
  type Painter = (Frame) => Unit;


  /** Transform the frame in which the painter is to be drawn, hence
   *  changing the appearance of the painter
   */
  def transformPainter(origin: Vector, newX: Vector, newY: Vector)(painter: Painter): Painter = {
    frame: Frame => {
      val newOrigin = frame.coordMap(origin);
      val newFrame = new Frame(newOrigin,
                               frame.coordMap(newX) - newOrigin,
                               frame.coordMap(newY) - newOrigin);
      painter(newFrame)
    }
  }


  /** Flip the painter vertically
   */
  def flipVert: Painter => Painter =
    transformPainter(new Vector(0.0, 1.0),
                     new Vector(1.0, 1.0),
                     new Vector(0.0, 0.0));

  /** Flip the painter horizontally
   */
  def flipHoriz: Painter => Painter =
    transformPainter(new Vector(1.0, 0.0),
                     new Vector(0.0, 0.0),
                     new Vector(1.0, 1.0));

  /** Compose a painter that draws p1 on the left of p2
   */
  def beside(p1: Painter, p2: Painter) : Painter = {
    frame: Frame => {
      transformPainter(new Vector(0.0, 0.0),
                       new Vector(0.5, 0.0),
                       new Vector(0.0, 1.0))(p1)(frame);
      transformPainter(new Vector(0.5, 0.0),
                       new Vector(1.0, 0.0),
                       new Vector(0.5, 1.0))(p2)(frame)
    }
  }

  /** Compose a painter that draws p1 below p2
   */
  def below(p1: Painter, p2: Painter): Painter = {
    frame: Frame => {
      transformPainter(new Vector(0.0, 0.0),
                       new Vector(1.0, 0.0),
                       new Vector(0.0, 0.5))(p1)(frame);
      transformPainter(new Vector(0.0, 0.5),
                       new Vector(1.0, 0.5),
                       new Vector(0.0, 1.0))(p2)(frame)
    }
  }

  def rightSplit(painter: Painter, n: Int): Painter = {
    if (n == 0) painter
    else {
      val smaller = rightSplit(painter, n-1);
      beside(painter, below(smaller, smaller))
    }
  }

  // A small test painter.
  def house(canvas: Graphics)(frame: Frame): Unit = {
    canvas.drawPolySegment(frame)(List(new Vector(0.0, 0.0),
                                       new Vector(1.0, 0.0),
                                       new Vector(1.0, 2.0/3.0),
                                       new Vector(0.0, 2.0/3.0),
                                       new Vector(0.5, 1.0),
                                       new Vector(1.0, 2.0/3.0),
                                       new Vector(0.0, 0.0),
                                       new Vector(0.0, 2.0/3.0),
                                       new Vector(1.0, 0.0)));
    canvas.repaint
  }

  def test = {
    val psfile = "-";
    val canvas: Graphics = new PostScript(psfile, 2, 2);

    // the identity frame
    val identFrame = new Frame(new Vector(0.0,0.0),
                               new Vector(1.0,0.0),
                               new Vector(0.0,1.0));

    // Create a basic painter...
    val p: Painter = house(canvas);
    // ...then compose it with itself.
    val threeHouses = beside(p, beside(p, flipVert(p)));

    // Use the painter to draw the final image.
    threeHouses(identFrame);

    // Don't forget to close the canvas!
    canvas.close
  }
}

//############################################################################

object Test {
  def main(args: Array[String]): Unit = {
    M0.test;
    ()
  }
}

//############################################################################
