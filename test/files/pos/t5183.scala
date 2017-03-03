trait Day

object Test {
  def foo(t: Int with Day) = t == t
}

class DayOps(val i: Int with Day) extends AnyVal

case class Test1(d: Int with Day)
case class Test2(d1: Int with Day, d2: Int with Day)

class User
class Checkin
object Example {
	  
	type Tagged[U] = { type Tag = U }
	type @@[T, U] = T with Tagged[U] // Thanks to @retronym for suggesting this type alias
	  
	class Tagger[U] {
	  def apply[T](t : T) : T @@ U = t.asInstanceOf[T @@ U]
	}
	def tag[U] = new Tagger[U]

	// Manual specialization needed here ... specializing apply above doesn't help
	def tag[U](i : Int) : Int @@ U = i.asInstanceOf[Int @@ U]
	def tag[U](l : Long) : Long @@ U = l.asInstanceOf[Long @@ U]
	def tag[U](d : Double) : Double @@ U = d.asInstanceOf[Double @@ U]

	def fetch[A](id: Int @@ A): A = null.asInstanceOf[A]

	def tag[U](arr: Array[Int]):Array[Int @@ U] = arr.asInstanceOf[Array[Int @@ U]]
	 
	tag[User](Array(3, 4, 5)).map(_.toString)
}