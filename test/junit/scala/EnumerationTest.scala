package scala.collection

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test

class EnumerationTest {

  @Test def t10906(): Unit = {
    object A extends Enumeration
    val s1 = A.values.map(x => 42)
    assertEquals(Set.empty, (s1: scala.collection.immutable.SortedSet[Int]))
    val s2 = A.values.map(x => x)
    assertEquals(Set.empty, (s2: A.ValueSet))
    val s3 = A.values.flatMap(x => Set(42))
    assertEquals(Set.empty, (s3: scala.collection.immutable.SortedSet[Int]))
    val s4 = A.values.flatMap(x => Set(x))
    assertEquals(Set.empty, (s4: A.ValueSet))
  }

  @Test def implicitOrderingIsSameAsEnumerationOrdering(): Unit = {
    object MyEnum extends Enumeration {
      val a, b, c = Value
    }
    assertSame(Ordering[MyEnum.Value], MyEnum.ValueOrdering)
  }
}
