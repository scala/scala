object Test extends scala.tools.partest.ReplTest {
  override def transformSettings(settings: scala.tools.nsc.Settings) = {
    settings.Yreplclassbased.value = true
    settings
  }

  def code = """
:paste < EOF
trait Show[-A] extends Any { def show(x: A): String }
object Show {
  implicit def shouty[Any]: Show[Any] =
    new Show[Any] { def show(x: Any) = if (x == null) "" else x.toString + "!" }
}
EOF

implicitly[Show[Int]] show 23

import scala.tools.nsc.interpreter.ReplPrinter
implicit def showReplPrinter[A](implicit z: Show[A]): ReplPrinter[A] = new ReplPrinter[A] {
  def print(x: A, maxElements: Int): String = {
    val s = z show x
    val nl = if (s contains "\n") "\n" else ""
    nl + s + "\n"
  }
}

// the lack of ! after 23 is expected because we don't enable custom string under -Yrepl-class-based
23
"""
}
