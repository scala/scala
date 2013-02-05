package life

 /*
 * Copied from http://rosettacode.org/wiki/Conway's_Game_of_Life/Scala
 */
class Coord private (val x: Int, val y: Int) {
  private val offsets = List(-1, 0, 1)
  private def offsetsOf(n: Int) = offsets map (_ + n)
 
  /**
   * A memoized list of all neighbors of a coordinate
   */
  lazy val neighbors = for {
    xn <- offsetsOf(x) if Coord.legal(xn)
    yn <- offsetsOf(y) if Coord.legal(yn) && (x, y) != (xn, yn)
  } yield Coord(xn, yn)
 
  // Coordinates can be used as offsets
  def +(c: Coord) = Coord(x + c.x, y + c.y)
  def -(c: Coord) = Coord(x - c.x, y - c.y)
 
  override def equals(other: Any) = other match {
    case that: Coord => this.x == that.x && this.y == that.y
    case _ => false
  }
  override def hashCode = ((x * 41) + y) * 41 + 41
  override def toString = "Coord(%d, %d)" format (x, y)
}

object Coord {
  // A Conway board is infinite in size; throw an exception if our hard limits are reached
  private def legal(n: Int) = {
    n.ensuring(Int.MinValue < _, "Coord too low").ensuring(_ < Int.MaxValue, "Coord too high")
    true
  }
  private val cache = new scala.collection.mutable.HashMap[(Int,Int), Coord]
 
  /**
   * Factory for coordinates. All coordinates are memoized.
   */
  def apply(x: Int, y: Int) = {
    require(legal(x) && legal(y))
    cache getOrElseUpdate ((x,y), new Coord(x, y))
  }
 
  /**
   * Coordinate extractor
   */
  def unapply(c: Coord) = Some((c.x, c.y))
 
  /**
   * An Ordering for coordinates which sorts by the X coordinate
   */
  val xOrdering = Ordering.fromLessThan((_: Coord).x < (_: Coord).x)
 
  /**
   * An Ordering for coordinates which sorts by the Y coordinate
   */
  val yOrdering = Ordering.fromLessThan((_: Coord).y < (_: Coord).y)
 
  /**
   * Any Tuple2[Int, Int] can be used as a Coordinate through this implict
   * conversion.
   */
  implicit def coordFromTuple(t: (Int, Int)) = apply(t._1, t._2)
}