package scala.reflect.internal.names

case class Term(s:String) extends NameBase(s)

class MapNamesTabelTest extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new MapNameTable[Term](Term.apply)
}
class ConcurrentMapNamesTabel1Test extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new ConcurrentMapNameTable1[Term](Term.apply)
}
class ConcurrentMapNamesTabel2Test extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new ConcurrentMapNameTable2[Term](Term.apply)
}
class ConcurrentMapNamesTabel3Test extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new ConcurrentMapNameTable3[Term](Term.apply)
}
class StrongConcurrentNodeInternerTest extends ExtendedNameTest  with ConcurrentNamesTest {
  override type T = Term
  override val nameTable: NameTable[T] = new StrongConcurrentNodeInterner[Term](Term.apply)
}
class WeakMapNameTableTest extends ExtendedNameTest with WeakNamesTest {
  override type T = Term
  override val nameTable: NameTable[T] = new WeakMapNameTable[Term](Term.apply)
}
class WeakAutoTrimConcurrentMapNameTable1Test extends ExtendedNameTest with WeakNamesTest {
  override type T = Term
  override val nameTable: NameTable[T] = new WeakAutoTrimConcurrentMapNameTable1[Term](Term.apply)
}
class WeakAutoTrimConcurrentMapNameTable2Test extends ExtendedNameTest with WeakNamesTest {
  override type T = Term
  override val nameTable: NameTable[T] = new WeakAutoTrimConcurrentMapNameTable2[Term](Term.apply)
}
class WeakNoAutoTrimConcurrentMapNameTable1Test extends ExtendedNameTest with WeakNamesTest {
  override type T = Term
  override val nameTable: WeakNoAutoTrimConcurrentMapNameTable1[T] = new WeakNoAutoTrimConcurrentMapNameTable1[Term](Term.apply)
  override def cleanupIfNeeded(): Unit = nameTable.trim()
}
class WeakAutoNoTrimConcurrentMapNameTable2Test extends ExtendedNameTest with WeakNamesTest {
  override type T = Term
  override val nameTable: WeakNoAutoTrimConcurrentMapNameTable2[T] = new WeakNoAutoTrimConcurrentMapNameTable2[Term](Term.apply)
  override def cleanupIfNeeded(): Unit = nameTable.trim()
}
class WeakFixedSizeNoAutoTrimConcurrentNodeInterner1Test extends ExtendedNameTest with WeakNamesTest {
  override type T = Term
  override val nameTable: WeakFixedSizeNoAutoTrimConcurrentNodeInterner[T] = new WeakFixedSizeNoAutoTrimConcurrentNodeInterner[Term](Term.apply)

  override def cleanupIfNeeded(): Unit = nameTable.trim()
}
class WeakFixedSizeNoAutoTrimConcurrentNodeInternerHashTest extends ExtendedNameTest with WeakNamesTest {
  override type T = Term
  override val nameTable: WeakFixedSizeNoAutoTrimConcurrentNodeInternerHash[T] = new WeakFixedSizeNoAutoTrimConcurrentNodeInternerHash[Term](Term.apply)

  override def cleanupIfNeeded(): Unit = nameTable.trim()
}
class WeakFixedSizeNoAutoTrimConcurrentNodeInternerNoHashTest extends ExtendedNameTest with WeakNamesTest {
  override type T = Term
  override val nameTable: WeakFixedSizeNoAutoTrimConcurrentNodeInternerNoHash[T] = new WeakFixedSizeNoAutoTrimConcurrentNodeInternerNoHash[Term](Term.apply)

  override def cleanupIfNeeded(): Unit = nameTable.trim()
}
class WeakFixedSizeAutoTrimConcurrentNodeInternerTest extends ExtendedNameTest with WeakNamesTest {
  override type T = Term
  override val nameTable: NameTable[T] = new WeakFixedSizeAutoTrimConcurrentNodeInterner[Term](Term.apply)

}
class Find0 extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new WeakFixedSizeAutoTrimConcurrentNodeInterner[Term](Term.apply) {
    override def find(chars: Array[Char], start: Int, count: Int): Term = super.find(chars, start, count)
  }
}
class Find1 extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new WeakFixedSizeAutoTrimConcurrentNodeInterner[Term](Term.apply) {
    override def nonAllocatingCharLookup: Boolean = true
    override def find(chars: Array[Char], start: Int, count: Int): Term = super.find1(chars, start, count)
  }
}
class Find2 extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new WeakFixedSizeAutoTrimConcurrentNodeInterner[Term](Term.apply) {
    override def find(chars: Array[Char], start: Int, count: Int): Term = super.find2(chars, start, count)
  }
}
class Find3 extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new WeakFixedSizeAutoTrimConcurrentNodeInterner[Term](Term.apply) {
    override def nonAllocatingCharLookup: Boolean = true
    override def find(chars: Array[Char], start: Int, count: Int): Term = super.find3(chars, start, count)
  }
}
class Safe extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new WeakFixedSizeAutoTrimConcurrentNodeInterner[Term](Term.apply)
}
class Unsafe extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new UnsafeWeakFixedSizeAutoTrimConcurrentNodeInterner[Term](Term.apply)
}
class Tail extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new WeakFixedSizeAutoTrimConcurrentNodeInterner[Term](Term.apply) {
    override def find(key: String): Term = super.find(key)
  }
}
class NoTail extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new WeakFixedSizeAutoTrimConcurrentNodeInterner[Term](Term.apply) {
    override def find(key: String): Term = super.findNoTail(key)
  }
}
