
/**
 * Checks that serialization of hash-based collections works correctly if the hashCode
 * changes on deserialization.
 */
object Test {

  import collection._
  def main(args: Array[String]) {
    for (i <- Seq(0, 1, 2, 10, 100)) {
      def entries = (0 until i).map(i => (new Foo, i)).toList
      def elements = entries.map(_._1)

      val maps = Seq[Map[Foo, Int]](new mutable.HashMap, new mutable.LinkedHashMap,
          immutable.HashMap.empty).map(_ ++ entries)
      test[Map[Foo, Int]](maps, entries.size, assertMap _)

      val sets = Seq[Set[Foo]](new mutable.HashSet, new mutable.LinkedHashSet,
          immutable.HashSet.empty).map(_ ++ elements)
      test[Set[Foo]](sets, entries.size, assertSet _)
    }
  }

  private def test[A <: AnyRef](collections: Seq[A], expectedSize: Int, assertFunction: (A, Int) => Unit) {
    for (collection <- collections) {
      assertFunction(collection, expectedSize)

      val bytes = toBytes(collection)
      Foo.hashCodeModifier = 1
      val deserializedCollection = toObject[A](bytes)

      assertFunction(deserializedCollection, expectedSize)
      assert(deserializedCollection.getClass == collection.getClass,
          "collection class should remain the same after deserialization ("+deserializedCollection.getClass+" != "+collection.getClass+")")
      Foo.hashCodeModifier = 0
    }
  }

  private def toObject[A](bytes: Array[Byte]): A = {
    val in = new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(bytes))
    in.readObject.asInstanceOf[A]
  }

  private def toBytes(o: AnyRef): Array[Byte] = {
    val bos = new java.io.ByteArrayOutputStream
    val out = new java.io.ObjectOutputStream(bos)
    out.writeObject(o)
    out.close
    bos.toByteArray
  }

  private def assertMap[A, B](map: Map[A, B], expectedSize: Int) {
    assert(expectedSize == map.size, "expected map size: " + expectedSize + ", actual size: " + map.size)
    map.foreach { case (k, v) =>
      assert(map.contains(k), "contains should return true for key in the map, key: " + k)
      assert(map(k) == v)
    }
  }

  private def assertSet[A](set: Set[A], expectedSize: Int) {
    assert(expectedSize == set.size, "expected set size: " + expectedSize + ", actual size: " + set.size)
    set.foreach { e => assert(set.contains(e), "contains should return true for element in the set, element: " + e) }
  }

  object Foo {
    /* Used to simulate a hashCode change caused by deserializing an instance with an
     * identity-based hashCode in another JVM.
     */
    var hashCodeModifier = 0
  }

  class Foo extends Serializable {
    override def hashCode = System.identityHashCode(this) + Foo.hashCodeModifier
  }
}
