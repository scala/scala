
object Test {

  import scala.testing.SUnit._
  import scala.collection.mutable.{ArrayBuffer, Buffer, BufferProxy, ListBuffer}

  trait BufferTest extends Assert {
    def doTest(x:Buffer[String]) = {
      // testing method += 
      x += "one"
      assertEquals("retrieving 'one'", x(0), "one")
      assertEquals("length A ", x.length, 1)
      x += "two"
      assertEquals("retrieving 'two'", x(1), "two")
      assertEquals("length B ", x.length, 2)

      // testing method -= (removing last element)
      x -=  "two"

      assertEquals("length C ", x.length, 1)

      try { x(1); fail("no exception for removed element") } 
      catch { case i:IndexOutOfBoundsException => }

      try { x.remove(1); fail("no exception for removed element") } 
      catch { case i:IndexOutOfBoundsException => }
      
      x += "two2"
      assertEquals("length D ", x.length, 2)

      // removing first element
      x.remove(0)
      assertEquals("length E ", x.length, 1)

      // toList
      assertEquals("toList ", x.toList, List("two2"))

      // clear
      x.clear
      assertEquals("length F ", x.length, 0)
      
      // copyToBuffer
      x += "a"
      x += "b"
      val dest = new ArrayBuffer[String]
      x copyToBuffer dest
      assertEquals("dest", List("a", "b"), dest.toList)
      assertEquals("source", List("a", "b"), x.toList)
    }
  }

  class TArrayBuffer extends TestCase("collection.mutable.ArrayBuffer") with Assert with BufferTest {

    var x: ArrayBuffer[String] = _

    override def runTest = { setUp; doTest(x); tearDown }
    
    override def setUp = { x = new scala.collection.mutable.ArrayBuffer }

    override def tearDown = { x.clear; x = null }
  }

  class TListBuffer extends TestCase("collection.mutable.ListBuffer") with Assert with BufferTest {

    var x: ListBuffer[String] = _

    override def runTest = { setUp; doTest(x); tearDown }

    override def setUp = { x = new scala.collection.mutable.ListBuffer }

    override def tearDown = { x.clear; x = null }

  }

  class TBufferProxy extends TestCase("collection.mutable.BufferProxy") with Assert with BufferTest {

    class BBuf(z:ListBuffer[String]) extends BufferProxy[String] {
      def self = z
    }

    var x: BufferProxy[String] = _

    override def runTest = { setUp; doTest(x); tearDown }

    override def setUp = { x = new BBuf(new scala.collection.mutable.ListBuffer) }

    override def tearDown = { x.clear; x = null }

  }

  def main(args:Array[String]) = {
    val ts = new TestSuite(
      //new TArrayBuffer, 
      new TListBuffer//, 
      //new TBufferProxy
    )
    val tr = new TestResult()
    ts.run(tr)
    for (failure <- tr.failures) {
      Console.println(failure)
    }
  }
}
