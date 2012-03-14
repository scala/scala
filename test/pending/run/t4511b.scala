import scala.{specialized => spec}

class Interval[@spec(Int) T](high:T)

class X1[@spec(Int) T](interval:Interval[T]) { val x = interval }
class Y1[@spec(Int) T](interval:Interval[T]) { val y = Some(interval) }

class X2[T](val interval:Interval[T]) { val x = interval }
class Y2[T](val interval:Interval[T]) { val y = Some(interval) }

class X3[@spec(Int) T](val interval:Interval[T]) { val x = interval }
class Y3[@spec(Int) T](val interval:Interval[T]) { val y = Some(interval) }

object Test {
 def tryit(o: => Any) = println(try { "ok: " + o.getClass.getName } catch { case e => "FAIL: " + e + "\n" + e.getStackTrace.mkString("\n  ") })

 def main(args: Array[String]) {
   tryit(new X1(new Interval(3)))
   tryit(new X2(new Interval(3)))
   tryit(new X3(new Interval(3)))
   tryit(new Y1(new Interval(3)))
   tryit(new Y2(new Interval(3)))
   tryit(new Y3(new Interval(3)))
 }
}
