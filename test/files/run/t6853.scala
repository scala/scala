// Test cases: the only place we can cut and paste without crying
// ourself to sleep.
object Test {

  def main(args: Array[String]): Unit = {
    // First testing the basic operations
    val m = collection.mutable.ListMap[String, Int]()
    var i = 0
    while(i < 2) { m += ("foo" + i) -> i; i = i+1}
    assert(m == Map("foo1"->1,"foo0"->0))
    m-= "foo0"
    assert(m == Map("foo1"->1))
    // Now checking if it scales as described in SI-6853
    i = 0
    while(i < 80000) { m += ("foo" + i) -> i; i = i+1}
    assert(m.size == 80000)
  }
}
