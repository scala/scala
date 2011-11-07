class Pos

class Super

object Super {
  implicit def pos2int(p: Pos): Int = 0
}  

object Sub extends Super {
  class Plus(x: Any) {
    def +(y: String): String = x.toString + y
  }
  implicit def any2plus(x: Any): Plus = new Plus(x)
}  

object Test {
  import Super._
  import Sub._
  val p = new Pos
  def f(x: Int): Int = x
  f(p+1)
}

object test2 {
  sealed trait HMap {
    def +[T](v: T) = HSome(v,this)
  }

  final case class HSome[T, L <: HMap](head: T, tail: L) extends HMap

  final object HEmpty extends HMap

  val set = HEmpty + 3 + "3"
  implicit def select[T](t: HSome[T,_]) = t.head
  implicit def selectTail[L](t: HSome[_,L]) = t.tail
  
  def foo(x: Int) = 3
  foo(set)
}

// #2180
class Mxml {

    private def processChildren( children:Seq[Any] ):List[Mxml] = {

        children.toList.flatMap ( e => {

            e match {

                case s:scala.collection.Traversable[_] => s case a => List(a)

            }

        })

    }

} 
