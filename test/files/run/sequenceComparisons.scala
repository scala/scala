import scala.collection.{ mutable, immutable }
import collection.{ Seq, Traversable }

object Test {
  // TODO: 
  //
  // SeqProxy
  // SeqForwarder
  // the commented out ones in seqMakers
  
  val seqMakers = List[List[Int] => Seq[Int]](
    // scala.Array(_: _*),
    mutable.ArrayBuffer(_: _*),
    // mutable.ArrayStack(_: _*),
    mutable.Buffer(_: _*),
    mutable.LinearSeq(_: _*),
    // null on Nil
    // mutable.LinkedList(_: _*),
    mutable.ListBuffer(_: _*),
    // mutable.PriorityQueue(_: _*),
    // immutable.Queue(_: _*),
    // mutable.Queue(_: _*),
    immutable.Seq(_: _*),
    mutable.Seq(_: _*),
    immutable.Stack(_: _*),
    // mutable.Stack(_: _*),    
    immutable.IndexedSeq(_: _*), // was Vector
    //mutable.Vector(_: _*),
    immutable.List(_: _*),
    immutable.Stream(_: _*)
  )
  
  abstract class Data[T] {
    val seq: Seq[T]
    private def seqList = seq.toList
    // _1 is inputs which must be true, _2 which must be false
    type Inputs = (List[List[T]], List[List[T]])
    case class Method(
      f: (Seq[T], Seq[T]) => Boolean,
      inputs: Inputs,
      descr: String
    ) {
      def trueList  = inputs._1
      def falseList = inputs._2
    }

    lazy val eqeq = Method(_ == _, (List(seqList), List(Nil, seqList drop 1, seqList ::: seqList)), "%s == %s")
    
    val startsWithInputs: Inputs
    lazy val startsWith = Method(_ startsWith _, startsWithInputs, "%s startsWith %s")
    
    val endsWithInputs: Inputs
    lazy val endsWith = Method(_ endsWith _, endsWithInputs, "%s endsWith %s")

    val indexOfSliceInputs: Inputs
    private def subseqTest(s1: Seq[T], s2: Seq[T]) = (s1 indexOfSlice s2) != -1
    lazy val indexOfSlice = Method(subseqTest _, indexOfSliceInputs, "(%s indexOfSlice %s) != -1")
    
    val sameElementsInputs: Inputs
    lazy val sameElements = Method(_ sameElements _, sameElementsInputs, "%s sameElements %s")
    
    def methodList = List(eqeq, startsWith, endsWith, indexOfSlice, sameElements)
  }
  
  object test1 extends Data[Int] {
    val seq = List(1,2,3,4,5)
    
    val startsWithInputs = (
      List(Nil, List(1), List(1,2), seq),
      List(List(1,2,3,4,6), seq ::: List(5), List(0))
    )
    
    val endsWithInputs = (
      List(Nil, List(5), List(4,5), seq),
      List(0 :: seq, List(5,2,3,4,5), List(3,4), List(5,6))
    )
    
    val indexOfSliceInputs = (
      List(Nil, List(1), List(3), List(5), List(1,2), List(2,3,4), List(4,5), seq),
      List(List(1,2,3,5), List(6), List(5,4,3,2,1), List(2,1))
    )
    
    val sameElementsInputs = (
      List(List(1,2,3,4,5)),
      List(Nil, List(1), List(1,2), List(2,3,4), List(2,3,4,5), List(2,3,4,5,1), List(1,2,3,5,4), seq reverse)
    )
  }
  
  val failures = new mutable.ListBuffer[String]
  var testCount = 0
  
  def assertOne(op1: Any, op2: Any, res: Boolean, str: String) {
    testCount += 1
    val resStr = str.format(op1, op2)
    // println(resStr)
    if (!res)
      failures += ("FAIL: " + resStr)
    // assert(res, resStr)
  }
  
  def runSeqs() = {
    for (s1f <- seqMakers ; s2f <- seqMakers ; testData <- List(test1)) {
      import testData._
      val scrut = s1f(seq)
      
      for (Method(f, (trueList, falseList), descr) <- methodList) {
        for (s <- trueList; val rhs = s2f(s))
          assertOne(scrut, rhs, f(scrut, rhs), descr)
        
        for (s <- falseList; val rhs = s2f(s))
          assertOne(scrut, rhs, !f(scrut, rhs), "!(" + descr + ")")
      }
    }
  }
  
  def main(args: Array[String]) {
    runSeqs()
    
    assert(failures.isEmpty, failures mkString "\n")
  }
}
