import scala.collection.immutable.HashMap

object Test extends App {

  case class Collision(value: Int) extends Ordered[Collision] {
    def compare(that: Collision) = value compare that.value

    override def hashCode = value / 5
  }

  def testCorrectness[T: Ordering](n: Int, mkKey: Int => T) {
    val o = implicitly[Ordering[T]]
    val s = HashMap.empty[T, Unit] ++ (0 until n).map(x => mkKey(x) -> (()))
    for (i <- 0 until n) {
      val ki = mkKey(i)
      val a = s.filter(kv => o.lt(kv._1, ki))
      val b = s.filterNot(kv => o.lt(kv._1, ki))
      require(a.size == i && (0 until i).forall(i => a.contains(mkKey(i))))
      require(b.size == n - i && (i until n).forall(i => b.contains(mkKey(i))))
    }
  }

  // this tests the structural sharing of the new filter
  // I could not come up with a simple test that tests structural sharing when only parts are reused, but
  // at least this fails with the old and passes with the new implementation
  def testSharing() {
    val s = HashMap.empty[Int, Unit] ++ (0 until 100).map(_ -> (()))
    require(s.filter(_ => true) eq s)
    require(s.filterNot(_ => false) eq s)
  }

  // this tests that neither hashCode nor equals are called during filter
  def testNoHashing() {
    var hashCount = 0
    var equalsCount = 0
    case class HashCounter(value: Int) extends Ordered[HashCounter] {
      def compare(that: HashCounter) = value compare that.value

      override def hashCode = {
        hashCount += 1
        value
      }

      override def equals(that: Any) = {
        equalsCount += 1
        that match {
          case HashCounter(value) => this.value == value
          case _ => false
        }
      }
    }

    val s = HashMap.empty[HashCounter, Unit] ++ (0 until 100).map(k => HashCounter(k) -> (()))
    val hashCount0 = hashCount
    val equalsCount0 = equalsCount
    val t = s.filter(_._1 < HashCounter(50))
    require(hashCount == hashCount0)
    require(equalsCount == equalsCount0)
  }

  // this tests correctness of filter and filterNot for integer keys
  testCorrectness[Int](100, identity _)
  // this tests correctness of filter and filterNot for keys with lots of collisions
  // this is necessary because usually collisions are rare so the collision-related code is not thoroughly tested
  testCorrectness[Collision](100, Collision.apply _)
  testSharing()
  testNoHashing()
}
