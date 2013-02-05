package life


 /*
 * Copied & adapted from http://rosettacode.org/wiki/Conway's_Game_of_Life/Scala
 */
object ConwayPatterns extends ConwayPatterns
class ConwayPatterns {

  def stillLives   = List(block, beehive, loaf, boat) map ((_, 1))
  def oscillators  = oscillators2.map((_, 2)) ::: oscillators3.map((_, 3)) 
  def oscillators2 = List(blinker, toad, beacon)
  def oscillators3 = List(pulsar)
  def spaceships   = List(glider, LWSS) map ((_, 4)) 
  def methuselahs  = List((diehard, 130), (acorn, 5206), (rPentomino, 1103))
  def guns         = List((gliderGun, 1234))

  def allPatternsByGroup = List[(String,List[((String,String),Int)])] (
       "stillLives"  -> stillLives
     , "oscillators" -> oscillators
     , "spaceships"  -> spaceships
     , "methuselahs" -> methuselahs
     , "guns"        -> guns
  )
  def allPatterns = allPatternsByGroup.map(_._2).flatten.map(_._1)
  
  // Still Lives patterns
  val block = "block" ->
              """|
                 | XX
                 | XX
                 |"""
  val beehive = "beehive" ->
              """|
                   |  XX
                   | X  X
                   |  XX
                   |"""
  val loaf = "loaf" ->
              """|
                |  XX
                | X  X
                |  X X
                |   X
                |"""
  val boat = "boat" ->
              """|
                | XX
                | X X
                |  X
                |"""
 
  // Oscillators patterns
  val blinker = "blinker" ->
                """|
                   |
                   | XXX
                   |
                   |"""
  val toad = "toad" ->
             """|
                |
                |  XXX
                | XXX
                |
                |"""
  val beacon = "beacon" ->
               """|
                  | XX
                  | XX
                  |   XX
                  |   XX
                  |"""
  val pulsar = "pulsar" ->
               """|
                  |
                  |    XXX   XXX
                  |
                  |  X    X X    X
                  |  X    X X    X
                  |  X    X X    X
                  |    XXX   XXX
                  |
                  |    XXX   XXX
                  |  X    X X    X
                  |  X    X X    X
                  |  X    X X    X
                  |
                  |    XXX   XXX
                  |
                  |"""
 
  // Spaceship patterns
  val glider = "glider" ->
               """|
                  |   X
                  | X X
                  |  XX
                  |"""
  val LWSS = "LWSS" ->
             """|
                |
                |  XXXX
                | X   X
                |     X
                | X  X
                |"""
 
  // Methuselah patterns
  val diehard = "diehard" ->
                """|
                   |       X
                   | XX
                   |  X   XXX
                   |"""
 
  val acorn = "acorn" ->
              """|
                 |  X
                 |    X
                 | XX  XXX
                 |"""
 
  val rPentomino = "rPentomino" ->
                   """|
                      | XX
                      |  XX
                      |  X
                      |"""
  
  val gliderGun = "gliderGun" ->
                   """|
                      |                         x
                      |                       x x         
                      |             xx      xx            xx
                      |            x   x    xx            xx
                      | xx        x     x   xx
                      | xx        x   x xx    x x 
                      |           x     x       x
                      |            x   x
                      |             xx
                      |"""
 
  // Helper methods
  // Enable constructing sets of coordinates from string patterns.
  implicit def coordsFromPattern(pattern: String) = for {
    (xs, y) <- pattern.stripMargin.split('\n').map(_.zipWithIndex).zipWithIndex.iterator
    (c, x) <- xs.iterator
    if c != ' '
  } yield Coord(x, y)
 

  // Move a set of coordinates to a point
  def moveTo(pattern: String, to: Coord) = (pattern: Iterator[Coord]) map (_ + to)
  def moveTo(coords: Iterator[Coord], to: Coord) = coords map (_ + to)
  def moveTo(coords: Traversable[Coord], to: Coord) = coords map (_ + to)
  
}