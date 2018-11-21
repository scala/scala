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

    val ping = c.untypecheck(c.inferImplicitValue(appliedType(definitions.ByNameParamClass, weakTypeOf[Ping]), silent = false))
    val pong = c.untypecheck(c.inferImplicitValue(appliedType(definitions.ByNameParamClass, weakTypeOf[Pong]), silent = false))

    q"""
      new Quux {
         def ping = $ping
         def pong = $pong
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

    val quux = c.untypecheck(c.inferImplicitValue(appliedType(definitions.ByNameParamClass, weakTypeOf[Quux]), silent = false))

    q"""
      new Ping {
         def next = $quux
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

    val quux = c.untypecheck(c.inferImplicitValue(appliedType(definitions.ByNameParamClass, weakTypeOf[Quux]), silent = false))

    q"""
      new Pong {
         def next = $quux
      }
    """
  }
}
