object Test extends App {
  def needPackageConcurrentMap(map: collection.concurrent.Map[Int, Int]): Unit = {
  }
  def needJavaConcurrent(map: java.util.concurrent.ConcurrentMap[Int, Int]): Unit = {
  }

  def testConversions(): Unit = {
    import collection.convert.ImplicitConversions._
    val skiplist = new java.util.concurrent.ConcurrentSkipListMap[Int, Int]
    val ctrie = new collection.concurrent.TrieMap[Int, Int]

    needPackageConcurrentMap(skiplist)
    needJavaConcurrent(ctrie)
  }

  def testConverters(): Unit = {
    import collection.JavaConverters._
    val skiplist = new java.util.concurrent.ConcurrentSkipListMap[Int, Int]
    val ctrie = new collection.concurrent.TrieMap[Int, Int]

    needPackageConcurrentMap(skiplist.asScala)
    needJavaConcurrent(ctrie.asJava)
  }

  testConversions()
  testConverters()
}
