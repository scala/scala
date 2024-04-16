//> using options -Wunused:nowarn
import annotation.nowarn
object T {
  @deprecated def f = 1
  def t1 = /
  @nowarn def t2 = f
}
