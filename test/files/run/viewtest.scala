object Test extends App {
  import collection._
  val xs: View[(String, Int)] = List("x").view.zip(LazyList.from(0))
  println(xs)

  val ys = List(1, 2, 3).view map { x => println("mapping "+x); x + 1 }
  println("ys defined")
  println(ys.head)
  println(ys.tail)
  println(ys(2))
  println(ys)
  println(ys.toIndexedSeq)

  val zs = Array(1, 2, 3).view
  val as: SeqView[Int] = zs map (_ + 1)
  val bs: IndexedSeq[Int] = as.toIndexedSeq
  val cs = zs.reverse
  assert(cs.toIndexedSeq == List(3, 2, 1))
  assert(zs(2) == 3)
  assert(bs == List(2, 3, 4))
}

/* crash confirmed.
2.8 regression: CCE when zipping list projection with stream
Reported by: 	szeiger 	Owned by: 	odersky
Priority: 	normal 	Component: 	Standard Library
Keywords: 	collections, zip 	Cc:
Fixed in version:
Description

Welcome to Scala version 2.8.0.r18784-b20090925021043 (Java HotSpot(TM) Client VM, Java 1.6.0_11).
Type in expressions to have them evaluated.
Type :help for more information.

scala> List("x").view.zip(Stream.from(0))List("x").view.zip(Stream.from(0))
java.lang.ClassCastException: scala.collection.generic.IterableViewTemplate$$anon$8 cannot be cast to scala.collection.generic.SequenceView
        at .<init>(<console>:5)
        at .<clinit>(<console>)
        at RequestResult$.<init>(<console>:4)
        at RequestResult$.<clinit>(<console>)
        at RequestResult$result(<console>)
        at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
        at sun.reflect.Nat...
*/
