import scala.annotation.meta._

class Bippy extends scala.annotation.StaticAnnotation

abstract class Foo {
  @Bippy           val x1: Int    // warn
  @(Bippy @field)  val x2: Int    // warn
  @(Bippy @getter) val x3: Int    // no warn
  @(Bippy @setter) val x4: Int    // warn
  @(Bippy @param)  val x5: Int    // warn
}

object Bar extends Foo {
  val x1 = 1
  val x2 = 2
  val x3 = 3
  val x4 = 4
  val x5 = 5

  @(Bippy @getter) private[this] val q1: Int = 1 // warn
  @(Bippy @getter) private val q2: Int = 1       // no warn

  def f1(@(Bippy @param) x: Int): Int = 0   // no warn
  def f2(@(Bippy @getter) x: Int): Int = 0  // warn - todo
  def f3(@(Bippy @setter) x: Int): Int = 0  // warn - todo
  def f4(@(Bippy @field) x: Int): Int = 0   // warn - todo
  def f5(@Bippy x: Int): Int = 0            // no warn

  @(Bippy @companionClass)  def g1(x: Int): Int = 0   // warn - todo
  @(Bippy @companionObject) def g2(x: Int): Int = 0   // warn - todo
  @(Bippy @companionMethod) def g3(x: Int): Int = 0   // no warn
                     @Bippy def g4(x: Int): Int = 0   // no warn

  @(Bippy @companionObject @companionMethod) def g5(x: Int): Int = 0   // no warn
}

class Dingo(
  @Bippy p0: Int,             // no warn
  @(Bippy @param) p1: Int,    // no warn
  @(Bippy @getter) p2: Int,   // warn
  @(Bippy @setter) p3: Int,   // warn
  @(Bippy @field) p4: Int     // warn
)

class ValDingo(
  @Bippy val p0: Int,             // no warn
  @(Bippy @param) val p1: Int,    // no warn
  @(Bippy @getter) val p2: Int,   // no warn
  @(Bippy @setter) val p3: Int,   // warn - todo
  @(Bippy @field) val p4: Int     // no warn
)

class VarDingo(
  @Bippy var p0: Int,             // no warn
  @(Bippy @param) var p1: Int,    // no warn
  @(Bippy @getter) var p2: Int,   // no warn
  @(Bippy @setter) var p3: Int,   // no warn
  @(Bippy @field) var p4: Int     // no warn
)

case class CaseDingo(
  @Bippy  p0: Int,            // no warn
  @(Bippy @param) p1: Int,    // no warn
  @(Bippy @getter) p2: Int,   // no warn
  @(Bippy @setter) p3: Int,   // warn - todo
  @(Bippy @field) p4: Int     // no warn
)
