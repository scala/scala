import scala.language.experimental.macros

import scala.reflect.macros.whitebox

object util {
  def lazily[T](implicit t: => T): T = t
}

abstract class Rec {
  def next: Rec
}

object Rec {
  implicit def mkRec: Rec = macro mkImpl

  def mkImpl(c: whitebox.Context): c.Tree = {
    import c.universe._

    q"""
      new Rec {
         def next = util.lazily[Rec]
      }
    """
  }
}
