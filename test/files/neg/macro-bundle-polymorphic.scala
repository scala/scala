import scala.language.experimental.macros
import scala.reflect.macros.blackbox.{Context => BlackboxContext}
import scala.reflect.macros.whitebox.{Context => WhiteboxContext}

class BlackboxBundle1[T](val c: BlackboxContext) {
  import c.universe._
  def impl = q"()"
}

class BlackboxBundle2[T <: BlackboxContext](val c: T) {
  import c.universe._
  def impl = q"()"
}

class BlackboxBundle3[T <: BlackboxContext, U <: T](val c: U) {
  import c.universe._
  def impl = q"()"
}

class WhiteboxBundle1[T](val c: WhiteboxContext) {
  import c.universe._
  def impl = q"()"
}

class WhiteboxBundle2[T <: WhiteboxContext](val c: T) {
  import c.universe._
  def impl = q"()"
}

class WhiteboxBundle3[T <: WhiteboxContext, U <: T](val c: U) {
  import c.universe._
  def impl = q"()"
}

object Macros {
  def black1: Any = macro BlackboxBundle1.impl
  def black2: Any = macro BlackboxBundle2.impl
  def black3: Any = macro BlackboxBundle3.impl

  def white1: Any = macro WhiteboxBundle1.impl
  def white2: Any = macro WhiteboxBundle2.impl
  def white3: Any = macro WhiteboxBundle3.impl
}