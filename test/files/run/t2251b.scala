class A
trait B[T <: B[T]] extends A
class B1[T <: B1[T]] extends B[T]
class C extends B[C] { override def toString = "C" }
class D extends B[D] { override def toString = "D" }
class E extends B[E] { override def toString = "E" }
class F extends B[F] { override def toString = "F" }
class G extends B1[G] { override def toString = "G" }

object Test {
  import scala.collection.{ mutable, immutable }
  import scala.collection.immutable.{ Vector }
  import scala.reflect.runtime.universe._
  def what[T: TypeTag](x: T) = println(typeTag[T])

  def main(args: Array[String]): Unit = {
//    what(List(List(new C), LazyList(new D))) // no longer works in 2.13, lub with a long refinement, no type tag available
    what(List(List(new C), LazyList(new D), Vector(new E), Set(new F)))
//    what(List(immutable.Vector(new C), LazyList(new D)))  // no longer works in 2.13, lub with a long refinement, no type tag available
    what(List(collection.Set(new F), mutable.Set(new G)))
    what(List(collection.Set(new F), immutable.Set(new G)))
    what(List(mutable.Set(new F), immutable.Set(new G)))
    what(List(mutable.Seq(new F), immutable.Seq(new G)))
    what(List(mutable.Map(new C -> new D), immutable.Map(new F -> new G)))
//    what(List(mutable.Queue(new F), immutable.List(new G))) // no longer works in 2.13, invalid lub computed (see test invalid-lubs.scala)
    what(List(mutable.Seq(new F), collection.Seq(new G)))
    what(List(immutable.LinearSeq(new F), collection.IndexedSeq(new G)))
  }
}


// class D extends B[D] { override def toString = "D" }


// class E {
//   val ys = List(List(new C), LazyList(new D))
// }

// object Test {
//   def trav = List(List(), LazyList())

//   def main(args: Array[String]): Unit = {
//     val f = (new E).ys _
//     var xs: Set[List[_ <: Seq[B[_]]]] = Set()
//     xs += f()
//     println(xs)
//   }
// }
