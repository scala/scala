import scala.util.Random

// Only y1 (val/val) should actually compile.
object Test {
  val r = new Random()

  val y1 : { val x : java.lang.Integer  } = new { val x = new java.lang.Integer(r.nextInt) }
  val w1 : y1.x.type = y1.x

  val y2 : { val x : java.lang.Integer  } = new { def x = new java.lang.Integer(r.nextInt) }
  val w2 : y2.x.type = y2.x

  val y3 : { def x : java.lang.Integer  } = new { val x = new java.lang.Integer(r.nextInt) }
  val w3 : y3.x.type = y3.x

  val y4 : { def x : java.lang.Integer  } = new { def x = new java.lang.Integer(r.nextInt) }
  val w4 : y4.x.type = y4.x
}
