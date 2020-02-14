class Foo {
  println(s"new Foo ${Foo.c}"); Foo.c += 1

  class Bar {
    println(s"new Bar ${Bar.c}"); Bar.c += 1
  }

  object Bar {
    var c = 0
    implicit def brr: Bar = new Bar
  }
  def m(implicit b: Bar): Foo = Foo.this
}
object Foo {
  var c = 0
}

object Test {
  (new Foo).m(null)
  def a = (new Foo).m(null)

  (new Foo).m
  def b = (new Foo).m

  (new Foo).m(null).m(null)
  def c = (new Foo).m(null).m(null)

  (new Foo).m.m
  def d = (new Foo).m.m

  { println("sara"); { println("frank"); new Foo}.m}.m
  def e = { println("sara"); { println("frank"); new Foo}.m}.m

  def main(args: Array[String]): Unit = {
    a
    b
    c
    d
    e
  }
}
