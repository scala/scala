import collection.mutable._
import reflect._


object Test extends App {
  def runTest[T, U](col: T)(clone: T => U)(mod: T => Unit)(implicit ct: ClassTag[T]): Unit = {
     val cloned = clone(col)
     assert(cloned == col, s"cloned should be equal to original. $cloned != $col")
     mod(col)
     assert(cloned != col, s"cloned should not modify when original does: $ct")
  }

  // Seqs
  runTest(ArrayBuffer(1,2,3))(_.clone) { buf => buf transform (_ + 1) }
  runTest(ArraySeq(1,2,3))(_.clone) { buf => buf transform (_ + 1) }
  runTest(Buffer(1,2,3))(_.clone) { buf => buf transform (_ + 1) }
  runTest(DoubleLinkedList(1,2,3))(_.clone) { buf => buf transform (_ + 1) }
  runTest(IndexedSeq(1,2,3))(_.clone) { buf => buf transform (_ + 1) }
  runTest(LinearSeq(1,2,3))(_.clone) { buf => buf transform (_ + 1) }
  runTest(LinkedList(1,2,3))(_.clone) { buf => buf transform (_ + 1) }
  runTest(ListBuffer(1,2,3))(_.clone) { buf => buf transform (_ + 1) }
  runTest(MutableList(1,2,3))(_.clone) { buf => buf transform (_ + 1) }
  runTest(Queue(1,2,3))(_.clone) { buf => buf transform (_ + 1) }
  runTest(Stack(1,2,3))(_.clone) { buf => buf transform (_ + 1) }

  // Sets
  runTest(BitSet(1,2,3))(_.clone) { buf => buf add 4 }
  runTest(HashSet(1,2,3))(_.clone) { buf => buf add 4 }
  runTest(Set(1,2,3))(_.clone) { buf => buf add 4 }
  runTest(SortedSet(1,2,3))(_.clone) { buf => buf add 4 }
  runTest(TreeSet(1,2,3))(_.clone) { buf => buf add 4 }

  // Maps
  runTest(HashMap(1->1,2->2,3->3))(_.clone) { buf => buf put (4,4) }
  runTest(WeakHashMap(1->1,2->2,3->3))(_.clone) { buf => buf put (4,4) }
}

