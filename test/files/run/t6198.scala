import scala.collection.immutable._

object Test extends App {
  // test that ListSet.tail does not use a builder
  // we can't test for O(1) behavior, so the best we can do is to
  // check that ls.tail always returns the same instance
  val ls = ListSet.empty[Int] + 1 + 2

  if(ls.tail ne ls.tail)
    println("ListSet.tail should not use a builder!")

  // class that always causes hash collisions
  case class Collision(value:Int) { override def hashCode = 0 }

  // create a set that should have a collison
  val x = HashSet.empty + Collision(0) + Collision(1)
  if(x.getClass.getSimpleName != "HashSetCollision1")
    println("HashSet of size >1 with collisions should use HashSetCollision")

  // remove the collision again by removing all but one element
  val y = x - Collision(0)
  if(y.getClass.getSimpleName != "HashSet1")
    println("HashSet of size 1 should use HashSet1" + y.getClass)
}
