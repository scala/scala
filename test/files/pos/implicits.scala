// #1435
object t1435 {
  implicit def a(s:String):String = error("")
  implicit def a(i:Int):String = error("")
  implicit def b(i:Int):String = error("")
}

class C1435 {
  val v:String = {
    import t1435.a
    2
  }
}

// #1579
object Test1579 {
  class Column
  class Query[E](val value: E)
  class Invoker(q: Any) { val foo = null }

  implicit def unwrap[C](q: Query[C]) = q.value
  implicit def invoker(q: Query[Column]) = new Invoker(q)

  val q = new Query(new Column)
  q.foo
}
// #1625
object Test1625 {

  class Wrapped(x:Any) {
    def unwrap() = x
  }

  implicit def byName[A](x: =>A) = new Wrapped(x)

  implicit def byVal[A](x: A) = x

  def main(args: Array[String]) = {

//    val res:Wrapped = 7 // works

    val res = 7.unwrap() // doesn't work

    println("=> result: " + res)
  }
}

object Test2188 {
  implicit def toJavaList[A: ClassManifest](t:collection.Seq[A]):java.util.List[A] = java.util.Arrays.asList(t.toArray:_*)

  val x: java.util.List[String] = List("foo")
}
