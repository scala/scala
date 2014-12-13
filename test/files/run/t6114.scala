object Test extends App {
  def testList = {
    val list = new java.util.ArrayList[Int]
    list.add(1)
    list.add(2)
    list.add(3)
    import scala.collection.JavaConverters._
    val next = list.asScala ++ List(4,5,6)
    assert(next != list.asScala)

    val raw = list.asScala
    val cloned = raw.clone
    list.add(1)
    assert(raw != cloned)
  }
  def testSet = {
    val set = new java.util.HashSet[Int]
    set.add(1)
    set.add(2)
    set.add(3)
    import scala.collection.JavaConverters._
    val next = set.asScala ++ Set(4,5,6)
    assert(next != set.asScala)

    val raw = set.asScala
    val cloned = raw.clone
    set.add(4)
    assert(raw != cloned)
  }
  def testMap = {
    val map = new java.util.HashMap[Int,Int]
    map.put(1,1)
    map.put(2,2)
    map.put(3,3)
    import scala.collection.JavaConverters._
    val next = map.asScala ++ Map(4->4,5->5,6->6)
    assert(next != map.asScala)

    val raw = map.asScala
    val cloned = raw.clone
    map.put(4,4)
    assert(raw != cloned)
  }

  def testCollection = {
    val list: java.util.Collection[Int] = new java.util.ArrayDeque[Int]
    list.add(1)
    list.add(2)
    list.add(3)
    import scala.collection.JavaConverters._
    val next = list.asScala ++ List(4,5,6)
    assert(next != list.asScala)

    // Note: Clone is hidden at this level, so no overridden cloning.
  }

  testList
  testSet
  testMap
  testCollection
}
