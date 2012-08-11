import scala.collection.immutable._

object Test extends App {

  // test that a HashTrieSet with one leaf element is not created!
  val x = HashSet.empty + 1 + 2
  if(x.getClass.getSimpleName != "HashTrieSet")
    println("A hash set containing two non-colliding values should be a HashTrieSet")

  val y = x - 1
  if(y.getClass.getSimpleName != "HashSet1")
    println("A hash set containing one element should always use HashSet1")

  // it is pretty hard to test that the case where a HashTrieSet has one element which
  // is itself of type HashTrieS t. That is because the improve hash function makes it very difficult
  // to find keys that will have hashes that are close together.
  //
  // However, it is also not necessary. Removing the ability of a HashTrieSet to have
  // one child of type HashTrieSet completely breaks the HashSet, so that many other
  // tests fail
}
