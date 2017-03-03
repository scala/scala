import collection.convert.ImplicitConversionsToScala._
import collection.JavaConverters._

object Test {

  def main(args: Array[String]) {
    test(new java.util.HashMap[String, String])
    test(new java.util.Properties)
    testConcMap
  }

  def testConcMap {
    import collection.convert.ImplicitConversionsToScala._

    val concMap = new java.util.concurrent.ConcurrentHashMap[String, String]

    test(concMap)
    val cmap = mapAsScalaConcurrentMap(concMap)
    cmap.putIfAbsent("absentKey", "absentValue")
    cmap.put("somekey", "somevalue")
    assert(cmap.remove("somekey", "somevalue") == true)
    assert(cmap.replace("absentKey", "newAbsentValue") == Some("absentValue"))
    assert(cmap.replace("absentKey", "newAbsentValue", ".......") == true)
  }

  def test(m: collection.mutable.Map[String, String]) {
    m.clear
    assert(m.size == 0)

    m.put("key", "value")
    assert(m.size == 1)

    assert(m.put("key", "anotherValue") == Some("value"))
    assert(m.put("key2", "value2") == None)
    assert(m.size == 2)

    m += (("key3", "value3"))
    assert(m.size == 3)

    m -= "key2"
    assert(m.size == 2)
    assert(m.nonEmpty)
    assert(m.remove("key") == Some("anotherValue"))

    m.clear
    for (i <- 0 until 10) m += (("key" + i, "value" + i))
    for ((k, v) <- m) assert(k.startsWith("key"))
  }
}






