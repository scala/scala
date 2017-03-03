// Test from http://lrytz.github.io/scala-aladdin-bugtracker/displayItem.do%3Fid=763.html
// and expanded with package object variants


trait Foo { type T; def apply() : T }
object e extends Foo { type T = Int; def apply() = 42 }

package p {
  trait T[X] { def O : { def apply(): X } }
  object `package` extends T[Int] {
    def O: { def apply(): Int } = new { def apply(): Int = 42 }
  }

  object Test {
    val x: Int = O()
  }
}

object Test {

  val f = new Foo { type T = Int; def apply() = 42 }

  def main(args: Array[String]): Unit = {
    val g = new Foo { type T = Int; def apply() = 42 }

    (e: Foo)()
    val ee: Int = e()

    (f: Foo)()
    val ff: Int = f()

    (g: Foo)()
    val gg: Int = g()

    val pp: Int = p.O()
  }
}
