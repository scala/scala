import scala.language.experimental.macros

import scala.reflect.macros.whitebox

object util {
  def lazily[T](implicit t: => T): T = t
}

abstract class Quux {
  def ping: Ping
  def pong: Pong
}

object Quux {
  implicit def mkQuux: Quux = macro mkImpl

  def mkImpl(c: whitebox.Context): c.Tree = {
    import c.universe._

    q"""
      new Quux {
         def ping = util.lazily[Ping]
         def pong = util.lazily[Pong]
      }
    """
  }
}

abstract class Ping {
  def next: Quux
}

object Ping {
  implicit def mkPing: Ping = macro mkImpl

  def mkImpl(c: whitebox.Context): c.Tree = {
    import c.universe._

    q"""
      new Ping {
         def next = util.lazily[Quux]
      }
    """
  }
}

abstract class Pong {
  def next: Quux
}

object Pong {
  implicit def mkPong: Pong = macro mkImpl

  def mkImpl(c: whitebox.Context): c.Tree = {
    import c.universe._

    q"""
      new Pong {
         def next = util.lazily[Quux]
      }
    """
  }
}
