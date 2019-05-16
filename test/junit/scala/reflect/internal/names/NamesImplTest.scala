package scala.reflect.internal.names

case class Term(s:String) extends NameBase(s)

class MapNamesTabelTest extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new MapNameTable[Term](Term.apply)
}
class ConcurrentMapNamesTabelTest extends ExtendedNameTest {
  override type T = Term
  override val nameTable: NameTable[T] = new ConcurrentMapNameTable[Term](Term.apply)
}
class StrongConcurrentNodeInternerTest extends ExtendedNameTest  with ConcurrentNamesTest {
  override type T = Term
  override val nameTable: NameTable[T] = new StrongConcurrentNodeInterner[Term](Term.apply)
}
class WeakMapNameTableTest extends ExtendedNameTest with WeakNamesTest {
  override type T = Term
  override val nameTable: NameTable[T] = new WeakMapNameTable[Term](Term.apply)
}
class WeakConcurrentMapNameTableTest extends ExtendedNameTest with WeakNamesTest {
  override type T = Term
  override val nameTable: NameTable[T] = new WeakConcurrentMapNameTable[Term](Term.apply)
}
class WeakFixedSizeNoAutoTrimConcurrentNodeInternerTest extends ExtendedNameTest with WeakNamesTest {
  override type T = Term
  override val nameTable: WeakFixedSizeNoAutoTrimConcurrentNodeInterner[T] = new WeakFixedSizeNoAutoTrimConcurrentNodeInterner[Term](Term.apply)

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
