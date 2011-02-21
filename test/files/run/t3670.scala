class C {
  val things = List("abcs")

  if (things.length < 30) {
    lazy val a = "a"
    println(a)
  }
  if (things.length < 30) {
    lazy val b = "b"
    println(b)
  }
}

class M extends App {
  def foo {
    lazy val a = {
        lazy val b = 1
        lazy val c = 2
        println(b)
        println(c)
    }
    a
    lazy val d = 42
    println(d)
  }
}

object Test extends App {
    new C()
    new M().foo
}
