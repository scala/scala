package scala.collection.concurrent

import org.junit.Test
import org.junit.Assert.assertEquals

import scala.util.hashing.Hashing
import scala.tools.testkit.AssertUtil.assertThrows

@deprecated("Tests deprecated API", since="2.13")
class TrieMapTest {

  private def check[T](result2: List[Any])(f: TrieMap[String, String] => IterableOnce[Any]) = {
    val m = TrieMap[String, String]()
    val values = f(m)
    m.put("k", "v")
    assertEquals(Nil, values.iterator.to(List))
    assertEquals(result2, f(m).iterator.to(List))
  }

  @Test
  def iterator(): Unit = {
    check(List(("k", "v")))(_.iterator)
  }

  @Test
  def values(): Unit = {
    check(List("v"))(_.values)
  }

  @Test
  def valuesIterator(): Unit = {
    check(List("v"))(_.valuesIterator)
  }

  @Test
  def keySet(): Unit = {
    check(List("k"))(_.keySet)
  }

  @Test
  def keysIterator(): Unit = {
    check(List("k"))(_.keysIterator)
  }

  @Test
  def keys(): Unit = {
    check(List("k"))(_.keys)
  }

  @Test
  def filterKeys(): Unit = {
    check(List(("k", "v")))(_.view.filterKeys(_ => true))
  }

  @Test
  def mapValues(): Unit = {
    check(List(("k", "v")))(_.view.mapValues(x => x))
  }

  @Test
  def customHashingAndEquiv_10481(): Unit = {
    val h = new Hashing[Int] { def hash(i: Int) = i % 4 }
    val e = new Equiv[Int] { def equiv(x: Int, y: Int) = (x % 8) == (y % 8) }
    val xs = new TrieMap[Int, String](h, e)
    xs.put(0, "zero")
    assertEquals(Some("zero"), xs.get(0))
    assertEquals(Some("zero"), xs.get(8)) // 8 and 0 are equivalent keys according to our custom equiv
    xs.put(4, "four") // 4 and 0 have the same hash according to our custom hashing, but they
    // are different keys (collision)
    assertEquals(Some("zero"), xs.get(8))
    assertEquals(Some("four"), xs.get(4))
  }

  @Test
  def nullValues_t10765(): Unit = {
    val trieMap = TrieMap[String, String]("a" -> null)
    assertEquals(null, trieMap("a"))
    assertEquals(Some(null), trieMap.get("a"))
    assertEquals(true, trieMap.contains("a"))
    assertEquals(1, trieMap.size)
    assertEquals(true, trieMap.iterator.hasNext)
    assertEquals(("a", null), trieMap.iterator.next())
  }

  @Test
  def nullValuesUpdate(): Unit = {
    def newTrieMap = TrieMap[String, String]("a" -> null, (null,  "b"), "c" -> "c")

    def check(tm: TrieMap[String, String], key: String, beforeValue: Option[String], afterValue: String, resultingSet: Set[(String, String)]): Unit = {
      beforeValue match {
        case Some(b) =>
          assertEquals(b, tm(key))
          assertEquals(Some(b), tm.get(key))
        case None =>
          assertThrows[NoSuchElementException](tm(key))
          assertEquals(None, tm.get(key))
      }

      tm.update(key, afterValue)
      assertEquals(afterValue, tm(key))
      assertEquals(Some(afterValue), tm.get(key))

      assertEquals(resultingSet, tm.toSet)
    }

    /*
      Cases to test:

      1. update existing (non-null key -> non-null value) with non-null value
      2. update existing (non-null key -> non-null value) with null
      3. update existing (non-null key -> null value) with non-null value
      4. update existing (non-null key -> null value) with null

      5. update existing (null key -> non-null value) with non-null value
      6. update existing (null key -> non-null value) with null
      7. update existing (null key -> null value) with non-null value
      8. update existing (null key -> null value) with null

      9. update new non-null key with non-null value
      10. update new non-null key with null
      11. update new null key with non-null value
      12. update new null key with null
     */


    // 1.
    check(newTrieMap, "c", Some("c"), "d", Set("a" -> null, (null,  "b"), "c" -> "d"))
    // 2.
    check(newTrieMap, "c", Some("c"), null, Set("a" -> null, (null,  "b"), "c" -> null))
    // 3.
    check(newTrieMap, "a", Some(null), "a2", Set("a" -> "a2", (null,  "b"), "c" -> "c"))
    // 4.
    check(newTrieMap, "a", Some(null), null, Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 5.
    check(newTrieMap, null, Some("b"), "b2", Set("a" -> null, (null,  "b2"), "c" -> "c"))
    // 6.
    check(newTrieMap, null, Some("b"), null, Set("a" -> null, (null,  null), "c" -> "c"))
    // 7.
    check(newTrieMap + ((null, null)), null, Some(null), "new value", Set("a" -> null, (null, "new value"), "c" -> "c"))
    // 8.
    check(newTrieMap + ((null, null)), null, Some(null), null, Set("a" -> null, (null,  null), "c" -> "c"))

    // 9.
    check(newTrieMap, "d", None, "dd", Set("a" -> null, (null,  "b"), "c" -> "c", "d" -> "dd"))
    // 10.
    check(newTrieMap, "d", None, null, Set("a" -> null, (null,  "b"), "c" -> "c", "d" -> null))
    // 11.
    check(newTrieMap - null, null, None, "new value", Set("a" -> null, (null,  "new value"), "c" -> "c"))
    // 12.
    check(newTrieMap - null, null, None, null, Set("a" -> null, (null,  null), "c" -> "c"))
  }

  @Test
  def nullValuesPut(): Unit = {
    def newTrieMap = TrieMap[String, String]("a" -> null, (null,  "b"), "c" -> "c")

    def check(tm: TrieMap[String, String], key: String, beforeValue: Option[String], afterValue: String, resultingSet: Set[(String, String)]): Unit = {
      beforeValue match {
        case Some(b) =>
          assertEquals(b, tm(key))
          assertEquals(Some(b), tm.get(key))
        case None =>
          assertThrows[NoSuchElementException](tm(key))
          assertEquals(None, tm.get(key))
      }

      assertEquals(beforeValue, tm.put(key, afterValue))
      assertEquals(afterValue, tm(key))
      assertEquals(Some(afterValue), tm.get(key))

      assertEquals(resultingSet, tm.toSet)
    }

    /*
      Cases to test:

      1. update existing (non-null key -> non-null value) with non-null value
      2. update existing (non-null key -> non-null value) with null
      3. update existing (non-null key -> null value) with non-null value
      4. update existing (non-null key -> null value) with null

      5. update existing (null key -> non-null value) with non-null value
      6. update existing (null key -> non-null value) with null
      7. update existing (null key -> null value) with non-null value
      8. update existing (null key -> null value) with null

      9. update new non-null key with non-null value
      10. update new non-null key with null
      11. update new null key with non-null value
      12. update new null key with null
     */


    // 1.
    check(newTrieMap, "c", Some("c"), "d", Set("a" -> null, (null,  "b"), "c" -> "d"))
    // 2.
    check(newTrieMap, "c", Some("c"), null, Set("a" -> null, (null,  "b"), "c" -> null))
    // 3.
    check(newTrieMap, "a", Some(null), "a2", Set("a" -> "a2", (null,  "b"), "c" -> "c"))
    // 4.
    check(newTrieMap, "a", Some(null), null, Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 5.
    check(newTrieMap, null, Some("b"), "b2", Set("a" -> null, (null,  "b2"), "c" -> "c"))
    // 6.
    check(newTrieMap, null, Some("b"), null, Set("a" -> null, (null,  null), "c" -> "c"))
    // 7.
    check(newTrieMap + ((null, null)), null, Some(null), "new value", Set("a" -> null, (null, "new value"), "c" -> "c"))
    // 8.
    check(newTrieMap + ((null, null)), null, Some(null), null, Set("a" -> null, (null,  null), "c" -> "c"))

    // 9.
    check(newTrieMap, "d", None, "dd", Set("a" -> null, (null,  "b"), "c" -> "c", "d" -> "dd"))
    // 10.
    check(newTrieMap, "d", None, null, Set("a" -> null, (null,  "b"), "c" -> "c", "d" -> null))
    // 11.
    check(newTrieMap - null, null, None, "new value", Set("a" -> null, (null,  "new value"), "c" -> "c"))
    // 12.
    check(newTrieMap - null, null, None, null, Set("a" -> null, (null,  null), "c" -> "c"))
  }
  @Test
  def nullValuesPutIfAbsent(): Unit = {
    def newTrieMap = TrieMap[String, String]("a" -> null, (null,  "b"), "c" -> "c")

    def check(tm: TrieMap[String, String], key: String, beforeValue: Option[String], afterValue: String, resultingSet: Set[(String, String)]): Unit = {
      beforeValue match {
        case Some(b) =>
          assertEquals(b, tm(key))
          assertEquals(Some(b), tm.get(key))
        case None =>
          assertThrows[NoSuchElementException](tm(key))
          assertEquals(None, tm.get(key))
      }

      assertEquals(beforeValue, tm.putIfAbsent(key, afterValue))

      val expectedValueNow = beforeValue.getOrElse(afterValue)

      assertEquals(expectedValueNow, tm(key))
      assertEquals(Some(expectedValueNow), tm.get(key))
      assertEquals(resultingSet, tm.toSet)
    }
    /*
      Cases to test:

      1. getOrElseUpdate existing (non-null key -> non-null value) with non-null value
      2. getOrElseUpdate existing (non-null key -> non-null value) with null
      3. getOrElseUpdate existing (non-null key -> null value) with non-null value
      4. getOrElseUpdate existing (non-null key -> null value) with null

      5. getOrElseUpdate existing (null key -> non-null value) with non-null value
      6. getOrElseUpdate existing (null key -> non-null value) with null
      7. getOrElseUpdate existing (null key -> null value) with non-null value
      8. getOrElseUpdate existing (null key -> null value) with null

      9. getOrElseUpdate new non-null key with non-null value
      10. getOrElseUpdate new non-null key with null
      11. getOrElseUpdate new null key with non-null value
      12. getOrElseUpdate new null key with null
     */

    // 1.
    check(newTrieMap, "c", Some("c"), "d", Set("a" -> null, (null,  "b"), "c" -> "c"))
    // 2.
    check(newTrieMap, "c", Some("c"), null, Set("a" -> null, (null,  "b"), "c" -> "c"))
    // 3.
    check(newTrieMap, "a", Some(null), "a2", Set("a" -> null, (null,  "b"), "c" -> "c"))
    // 4.
    check(newTrieMap, "a", Some(null), null, Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 5.
    check(newTrieMap, null, Some("b"), "b2", Set("a" -> null, (null,  "b"), "c" -> "c"))
    // 6.
    check(newTrieMap, null, Some("b"), null, Set("a" -> null, (null,  "b"), "c" -> "c"))
    // 7.
    check(newTrieMap + ((null, null)), null, Some(null), "new value", Set("a" -> null, (null,  null), "c" -> "c"))
    // 8.
    check(newTrieMap + ((null, null)), null, Some(null), null, Set("a" -> null, (null,  null), "c" -> "c"))

    // 9.
    check(newTrieMap, "d", None, "dd", Set("a" -> null, (null,  "b"), "c" -> "c", "d" -> "dd"))
    // 10.
    check(newTrieMap, "d", None, null, Set("a" -> null, (null,  "b"), "c" -> "c", "d" -> null))
    // 11.
    check(newTrieMap - null, null, None, "new value", Set("a" -> null, (null,  "new value"), "c" -> "c"))
    // 12.
    check(newTrieMap - null, null, None, null, Set("a" -> null, (null,  null), "c" -> "c"))
  }

  @Test
  def nullValuesGetOrElseUpdate(): Unit = {

    def newTrieMap = TrieMap[String, String]("a" -> null, (null,  "b"), "c" -> "c")

    def check(tm: TrieMap[String, String], key: String, beforeValue: Option[String], afterValue: String, resultingSet: Set[(String, String)]): Unit = {
      beforeValue match {
        case Some(b) =>
          assertEquals(b, tm(key))
          assertEquals(Some(b), tm.get(key))
          assertEquals(b, tm.getOrElseUpdate(key, afterValue))
          assertEquals(b, tm(key))
          assertEquals(Some(b), tm.get(key))
        case None =>
          assertThrows[NoSuchElementException](tm(key))
          assertEquals(None, tm.get(key))

          assertEquals(afterValue, tm.getOrElseUpdate(key, afterValue))
          assertEquals(afterValue, tm(key))
          assertEquals(Some(afterValue), tm.get(key))
      }

      assertEquals(resultingSet, tm.toSet)
    }
    /*
      Cases to test:

      1. getOrElseUpdate existing (non-null key -> non-null value) with non-null value
      2. getOrElseUpdate existing (non-null key -> non-null value) with null
      3. getOrElseUpdate existing (non-null key -> null value) with non-null value
      4. getOrElseUpdate existing (non-null key -> null value) with null

      5. getOrElseUpdate existing (null key -> non-null value) with non-null value
      6. getOrElseUpdate existing (null key -> non-null value) with null
      7. getOrElseUpdate existing (null key -> null value) with non-null value
      8. getOrElseUpdate existing (null key -> null value) with null

      9. getOrElseUpdate new non-null key with non-null value
      10. getOrElseUpdate new non-null key with null
      11. getOrElseUpdate new null key with non-null value
      12. getOrElseUpdate new null key with null
     */

    // 1.
    check(newTrieMap, "c", Some("c"), "d", Set("a" -> null, (null,  "b"), "c" -> "c"))
    // 2.
    check(newTrieMap, "c", Some("c"), null, Set("a" -> null, (null,  "b"), "c" -> "c"))
    // 3.
    check(newTrieMap, "a", Some(null), "a2", Set("a" -> null, (null,  "b"), "c" -> "c"))
    // 4.
    check(newTrieMap, "a", Some(null), null, Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 5.
    check(newTrieMap, null, Some("b"), "b2", Set("a" -> null, (null,  "b"), "c" -> "c"))
    // 6.
    check(newTrieMap, null, Some("b"), null, Set("a" -> null, (null,  "b"), "c" -> "c"))
    // 7.
    check(newTrieMap + ((null, null)), null, Some(null), "new value", Set("a" -> null, (null,  null), "c" -> "c"))
    // 8.
    check(newTrieMap + ((null, null)), null, Some(null), null, Set("a" -> null, (null,  null), "c" -> "c"))

    // 9.
    check(newTrieMap, "d", None, "dd", Set("a" -> null, (null,  "b"), "c" -> "c", "d" -> "dd"))
    // 10.
    check(newTrieMap, "d", None, null, Set("a" -> null, (null,  "b"), "c" -> "c", "d" -> null))
    // 11.
    check(newTrieMap - null, null, None, "new value", Set("a" -> null, (null,  "new value"), "c" -> "c"))
    // 12.
    check(newTrieMap - null, null, None, null, Set("a" -> null, (null,  null), "c" -> "c"))
  }

  @Test
  def nullValuesReplaceKeyValue() = {

    def newTrieMap = TrieMap[String, String]("a" -> null, (null,  "b"), "c" -> "c")

    def check(tm: TrieMap[String, String], key: String, beforeValue: Option[String], afterValue: String, resultingSet: Set[(String, String)]): Unit = {
      beforeValue match {
        case Some(b) =>
          assertEquals(b, tm(key))
          assertEquals(Some(b), tm.get(key))
          assertEquals(Some(b), tm.replace(key, afterValue))
          assertEquals(afterValue, tm(key))
          assertEquals(Some(afterValue), tm.get(key))
        case None =>
          assertThrows[NoSuchElementException](tm(key))
          assertEquals(None, tm.get(key))

          assertEquals(None, tm.replace(key, afterValue))

          assertThrows[NoSuchElementException](tm(key))
          assertEquals(None, tm.get(key))
      }

      assertEquals(resultingSet, tm.toSet)
    }
    /*
      Cases to test:

      1. getOrElseUpdate existing (non-null key -> non-null value) with non-null value
      2. getOrElseUpdate existing (non-null key -> non-null value) with null
      3. getOrElseUpdate existing (non-null key -> null value) with non-null value
      4. getOrElseUpdate existing (non-null key -> null value) with null

      5. getOrElseUpdate existing (null key -> non-null value) with non-null value
      6. getOrElseUpdate existing (null key -> non-null value) with null
      7. getOrElseUpdate existing (null key -> null value) with non-null value
      8. getOrElseUpdate existing (null key -> null value) with null

      9. getOrElseUpdate new non-null key with non-null value
      10. getOrElseUpdate new non-null key with null
      11. getOrElseUpdate new null key with non-null value
      12. getOrElseUpdate new null key with null
     */

    // 1.
    check(newTrieMap, "c", Some("c"), "d", Set("a" -> null, (null,  "b"), "c" -> "d"))
    // 2.
    check(newTrieMap, "c", Some("c"), null, Set("a" -> null, (null,  "b"), "c" -> null))
    // 3.
    check(newTrieMap, "a", Some(null), "a2", Set("a" -> "a2", (null,  "b"), "c" -> "c"))
    // 4.
    check(newTrieMap, "a", Some(null), null, Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 5.
    check(newTrieMap, null, Some("b"), "b2", Set("a" -> null, (null,  "b2"), "c" -> "c"))
    // 6.
    check(newTrieMap, null, Some("b"), null, Set("a" -> null, (null,  null), "c" -> "c"))
    // 7.
    check(newTrieMap + ((null, null)), null, Some(null), "new value", Set("a" -> null, (null, "new value"), "c" -> "c"))
    // 8.
    check(newTrieMap + ((null, null)), null, Some(null), null, Set("a" -> null, (null,  null), "c" -> "c"))

    // 9.
    check(newTrieMap, "d", None, "dd", Set("a" -> null, (null,  "b"), "c" -> "c"))
    // 10.
    check(newTrieMap, "d", None, null, Set("a" -> null, (null,  "b"), "c" -> "c"))
    // 11.
    check(newTrieMap - null, null, None, "new value", Set("a" -> null, "c" -> "c"))
    // 12.
    check(newTrieMap - null, null, None, null, Set("a" -> null, "c" -> "c"))
  }

  @Test
  def nullValuesReplaceKeyOldValueNewValue() = {

    def newTrieMap = TrieMap[String, String]("a" -> null, (null,  "b"), "c" -> "c")

    def check(
      tm: TrieMap[String, String],
      key: String,
      beforeValue: Option[String],
      conditionalOldValue: String,
      afterValue: String,
      resultingSet: Set[(String, String)]): Unit = {

      beforeValue match {
        case Some(b) if b == conditionalOldValue =>
          assertEquals(b, tm(key))
          assertEquals(Some(b), tm.get(key))
          assertEquals(true, tm.replace(key, conditionalOldValue, afterValue))
          assertEquals(afterValue, tm(key))
          assertEquals(Some(afterValue), tm.get(key))
        case Some(b) =>
          assertEquals(b, tm(key))
          assertEquals(Some(b), tm.get(key))
          assertEquals(false, tm.replace(key, conditionalOldValue, afterValue))
          assertEquals(b, tm(key))
          assertEquals(Some(b), tm.get(key))
        case None =>
          assertThrows[NoSuchElementException](tm(key))
          assertEquals(None, tm.get(key))

          assertEquals(false, tm.replace(key, conditionalOldValue, afterValue))

          assertThrows[NoSuchElementException](tm(key))
          assertEquals(None, tm.get(key))
      }

      assertEquals(resultingSet, tm.toSet)
    }
    /*
      Cases to test:

      1a. replace existing (non-null key -> non-null value) with non-null value, when oldValue matches
      1b. same, but oldValue does not match

      2a. replace existing (non-null key -> non-null value) with null, when oldValue matches
      2b. same, but oldValue does not match

      3a. replace existing (non-null key -> null value) with non-null value, when oldValue matches
      3b. same, but oldValue does not match

      4a. replace existing (non-null key -> null value) with null, when oldValue matches
      4b. same, but oldValue does not match

      5a. replace existing (null key -> non-null value) with non-null value, when oldValue matches
      5b. same, but oldValue does not match

      6a. replace existing (null key -> non-null value) with null, when oldValue matches
      6b. same, but oldValue does not match

      7a. replace existing (null key -> null value) with non-null value, when oldValue matches
      7b. same, but oldValue does not match

      8a. replace existing (null key -> null value) with null, when oldValue matches
      8b. same, but oldValue does not match

      9a. replace new non-null key with non-null value

      10a. replace new non-null key with null

      11a. replace new null key with non-null value

      12a. replace new null key with null
     */

    // 1.
    check(newTrieMap, "c", Some("c"), "c", "d", Set("a" -> null, (null,  "b"), "c" -> "d"))

    check(newTrieMap, "c", Some("c"), null, "d", Set("a" -> null, (null,  "b"), "c" -> "c"))
    check(newTrieMap, "c", Some("c"), "not c", "d", Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 2.
    check(newTrieMap, "c", Some("c"), "c", null, Set("a" -> null, (null,  "b"), "c" -> null))

    check(newTrieMap, "c", Some("c"), "not c", null, Set("a" -> null, (null,  "b"), "c" -> "c"))
    check(newTrieMap, "c", Some("c"), null, null, Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 3.
    check(newTrieMap, "a", Some(null), null, "a2", Set("a" -> "a2", (null,  "b"), "c" -> "c"))

    check(newTrieMap, "a", Some(null), "not null", "a2", Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 4.
    check(newTrieMap, "a", Some(null), null, null, Set("a" -> null, (null,  "b"), "c" -> "c"))
    check(newTrieMap, "a", Some(null), "not null", null, Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 5.
    check(newTrieMap, null, Some("b"), "b", "b2", Set("a" -> null, (null,  "b2"), "c" -> "c"))

    check(newTrieMap, null, Some("b"), "not b", "b2", Set("a" -> null, (null,  "b"), "c" -> "c"))
    check(newTrieMap, null, Some("b"), null, "b2", Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 6.
    check(newTrieMap, null, Some("b"), "b", null, Set("a" -> null, (null,  null), "c" -> "c"))

    check(newTrieMap, null, Some("b"), "not b", null, Set("a" -> null, (null,  "b"), "c" -> "c"))
    check(newTrieMap, null, Some("b"), null, null, Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 7.
    check(newTrieMap + ((null, null)), null, Some(null), null, "new value", Set("a" -> null, (null, "new value"), "c" -> "c"))

    check(newTrieMap + ((null, null)), null, Some(null), "not null", "new value", Set("a" -> null, (null, null), "c" -> "c"))
    // 8.
    check(newTrieMap + ((null, null)), null, Some(null), null, null, Set("a" -> null, (null,  null), "c" -> "c"))

    check(newTrieMap + ((null, null)), null, Some(null), "not null", null, Set("a" -> null, (null,  null), "c" -> "c"))

    // 9.
    check(newTrieMap, "d", None, "not null","dd", Set("a" -> null, (null,  "b"), "c" -> "c"))
    check(newTrieMap, "d", None, null,"dd", Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 10.
    check(newTrieMap, "d", None, "not null", null, Set("a" -> null, (null,  "b"), "c" -> "c"))
    check(newTrieMap, "d", None, null, null, Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 11.
    check(newTrieMap - null, null, None, "not null","new value", Set("a" -> null, "c" -> "c"))
    check(newTrieMap - null, null, None, null,"new value", Set("a" -> null, "c" -> "c"))

    // 12.
    check(newTrieMap - null, null, None, "not null",null, Set("a" -> null, "c" -> "c"))
    check(newTrieMap - null, null, None, null,null, Set("a" -> null, "c" -> "c"))
  }

  @Test
  def nullValuesRemove() = {
    def newTrieMap = TrieMap[String, String]("a" -> null, (null,  "b"), "c" -> "c")

    def check(
      tm: TrieMap[String, String],
      key: String,
      beforeValue: Option[String],
      conditionalOldValue: String,
      resultingSet: Set[(String, String)]): Unit = {

      beforeValue match {
        case Some(b) if b == conditionalOldValue =>
          assertEquals(b, tm(key))
          assertEquals(Some(b), tm.get(key))
          assertEquals(true, tm.remove(key, conditionalOldValue))
          assertThrows[NoSuchElementException](tm(key))
          assertEquals(None, tm.get(key))
        case Some(b) =>
          assertEquals(b, tm(key))
          assertEquals(Some(b), tm.get(key))
          assertEquals(false, tm.remove(key, conditionalOldValue))
          assertEquals(b, tm(key))
          assertEquals(Some(b), tm.get(key))
        case None =>
          assertThrows[NoSuchElementException](tm(key))
          assertEquals(None, tm.get(key))

          assertEquals(false, tm.remove(key, conditionalOldValue))

          assertThrows[NoSuchElementException](tm(key))
          assertEquals(None, tm.get(key))
      }

      assertEquals(resultingSet, tm.toSet)
    }

    /*
      Cases to test:

      1. remove existing (non-null key -> non-null value)
      2. remove existing (non-null key -> null value)

      3. remove existing (null key -> non-null value)
      4. remove existing (null key -> null value)

      5. remove new non-null key
      61. remove new null key
     */

    // 1.
    check(newTrieMap, "c", Some("c"), "c", Set("a" -> null, (null,  "b")))
    check(newTrieMap, "c", Some("c"), "not c", Set("a" -> null, (null,  "b"), "c" -> "c"))
    check(newTrieMap, "c", Some("c"), null, Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 2.
    check(newTrieMap, "a", Some(null), null, Set((null,  "b"), "c" -> "c"))
    check(newTrieMap, "a", Some(null), "not null", Set("a" -> null,(null,  "b"), "c" -> "c"))

    // 3.
    check(newTrieMap, null, Some("b"), "b", Set("a" -> null, "c" -> "c"))
    check(newTrieMap, null, Some("b"), "not b", Set("a" -> null, (null, "b"), "c" -> "c"))
    check(newTrieMap, null, Some("b"), null, Set("a" -> null, (null, "b"), "c" -> "c"))

    // 4.
    check(newTrieMap + ((null, null)), null, Some(null), null, Set("a" -> null, "c" -> "c"))
    check(newTrieMap + ((null, null)), null, Some(null), "not null", Set("a" -> null, (null, null), "c" -> "c"))

    // 5.
    check(newTrieMap, "d", None, "does not exists", Set("a" -> null, (null,  "b"), "c" -> "c"))
    check(newTrieMap, "d", None, null, Set("a" -> null, (null,  "b"), "c" -> "c"))

    // 6.
    check(newTrieMap - null, null, None, "new value", Set("a" -> null, "c" -> "c"))
    check(newTrieMap - null, null, None, null, Set("a" -> null, "c" -> "c"))
  }

  @Test
  def testUpdateWith(): Unit = {
    val insertIfAbsent: Option[String] => Option[String] = _.orElse(Some("b"))
    val hashMap1 = TrieMap(1 -> "a")
    assertEquals(hashMap1.updateWith(1)(insertIfAbsent), Some("a"))
    assertEquals(hashMap1, TrieMap(1 -> "a"))
    val hashMap2 = TrieMap(1 -> "a")
    assertEquals(hashMap2.updateWith(2)(insertIfAbsent), Some("b"))
    assertEquals(hashMap2, TrieMap(1 -> "a", 2 -> "b"))

    val noneAnytime: Option[String] => Option[String] = _ => None
    val hashMap3 = TrieMap(1 -> "a")
    assertEquals(hashMap3.updateWith(1)(noneAnytime), None)
    assertEquals(hashMap3, TrieMap())
    val hashMap4 = TrieMap(1 -> "a")
    assertEquals(hashMap4.updateWith(2)(noneAnytime), None)
    assertEquals(hashMap4, TrieMap(1 -> "a"))
  }
}
