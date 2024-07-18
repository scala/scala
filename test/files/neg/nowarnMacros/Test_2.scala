//> using options -Werror

import language.experimental.macros
import scala.annotation.nowarn

object Test {
  def discard(expr: Any): Unit = macro Macro.discard

  def t1a = discard {
    try 1 // warn
    2
  }
  def t1b = discard {
    (try 1): @nowarn
    2
  }

  def t2a = discard {
    try 1 // warn
    2
  }
  @nowarn def t2b = discard {
    try 1
    2
  }

  def t3a = discard {
    try 1 // warn
    2
  }
  def t3b = discard {
    try 1 // warn
    2
  }: @nowarn
}
