package mixintest.c
import mixintest.a.A
import mixintest.b.B
case class C(override protected val x: Int) extends A with B