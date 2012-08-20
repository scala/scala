import scala.collection.immutable._

object Test extends App {

  def test1() {
    // test that a HashTrieMap with one leaf element is not created!
    val x = HashMap.empty + (1->1) + (2->2)
    if(x.getClass.getSimpleName != "HashTrieMap")
      println("A hash map containing two non-colliding values should be a HashTrieMap")

    val y = x - 1
    if(y.getClass.getSimpleName != "HashMap1")
      println("A hash map containing one element should always use HashMap1")
  }

  def test2() {
    // class that always causes hash collisions
    case class Collision(value:Int) { override def hashCode = 0 }

    // create a set that should have a collison
    val x = HashMap.empty + (Collision(0)->0) + (Collision(1) ->0)
    if(x.getClass.getSimpleName != "HashMapCollision1")
      println("HashMap of size >1 with collisions should use HashMapCollision")

    // remove the collision again by removing all but one element
    val y = x - Collision(0)
    if(y.getClass.getSimpleName != "HashMap1")
      println("HashMap of size 1 should use HashMap1" + y.getClass)
  }
  test1()
  test2()
}
