import scala.reflect.macros.blackbox.Context

object Impls {
  def one(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    val x1 = x orElse q"2"
    val y1 = y orElse q"-40"
    q"println(${c.macroApplication.toString + " = "} + ($x1 - $y1))"
  }

  def onezero(c: Context)(x: c.Tree, y: c.Tree)(z: c.Tree, w: c.Tree) = {
    import c.universe._
    val x1 = x orElse q"2"
    val y1 = y orElse q"-40"
    val z1 = z
    val w1 = w
    q"println(${c.macroApplication.toString + " = "} + ($x1 - $y1 + $z1 - $w1))"
  }

  def zeroone(c: Context)(x: c.Tree, y: c.Tree)(z: c.Tree, w: c.Tree) = {
    import c.universe._
    val x1 = x
    val y1 = y
    val z1 = z orElse q"2"
    val w1 = w orElse q"-40"
    q"println(${c.macroApplication.toString + " = "} + ($x1 - $y1 + $z1 - $w1))"
  }

  def oneone(c: Context)(x: c.Tree, y: c.Tree)(z: c.Tree, w: c.Tree) = {
    import c.universe._
    val x1 = x orElse q"2"
    val y1 = y orElse q"-40"
    val z1 = z orElse q"2"
    val w1 = w orElse q"-40"
    q"println(${c.macroApplication.toString + " = "} + ($x1 - $y1 + $z1 - $w1))"
  }
}