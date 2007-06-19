class Foo {
  class Line {
    case class Cell[T](var x: T)
    def f[T](x: Any): Cell[t1] for_some { type t1 } = x match { case y: Cell[t] => y }

    var x: Cell[T] for_some { type T } = new Cell(1)
    println({ x = new Cell("abc"); x })
  }
}

trait Counter[T] {
   def newCounter: T
   def get(i: T): Int
   def inc(i: T): T
}

case class C[T](x: T)

object LUB {
  def x = C(1)
  def y = C("abc")
  var coinflip: boolean = _
  def z = if (coinflip) x else y
  def zz: C[_1] for_some { type _1 >: Int with java.lang.String } = z
}
 object Test extends Application {

   def foo(x : Counter[T] { def name : String } for_some { type T }) = x match {
     case ctr: Counter[t] =>
       val c = ctr.newCounter
       println(ctr.name+" "+ctr.get(ctr.inc(ctr.inc(c))))
     case _ =>
   }

   var ex: Counter[T] for_some { type T } = _
   ex = ci
   ex = cf

   val ci = new Counter[Int] {
     def newCounter = 0
     def get(i: Int) = i
     def inc(i: Int) = i+1
     def name = "Int"
   }

   val cf = new Counter[Float] {
     def newCounter = 0
     def get(i: Float) = i.intValue
     def inc(i: Float) = i+1
     def name = "Float"
   }

   foo(ci)
   foo(cf)
   val foo = new Foo
   new foo.Line
}
