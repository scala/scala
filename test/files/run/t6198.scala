import scala.collection.immutable._

object Test extends App {

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
