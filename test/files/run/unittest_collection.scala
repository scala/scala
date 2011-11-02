object Test {

  import scala.collection.mutable.{ArrayBuffer, Buffer, BufferProxy, ListBuffer}

  def main(args: Array[String]) {
    test(collection.mutable.ArrayBuffer[String]())
    test(collection.mutable.ListBuffer[String]())
    class BBuf(z:ListBuffer[String]) extends BufferProxy[String] {
      def self = z
    }
    test(new BBuf(collection.mutable.ListBuffer[String]()))
  }

  def test(x: Buffer[String]) {
    // testing method += 
    x += "one"
    assert(x(0) == "one", "retrieving 'one'")
    assert(x.length == 1, "length A")
    x += "two"
    assert(x(1) == "two", "retrieving 'two'")
    assert(x.length == 2, "length B")

    // testing method -= (removing last element)
    x -=  "two"

    assert(x.length == 1, "length C")

    try { x(1); sys.error("no exception for removed element") } 
    catch { case i:IndexOutOfBoundsException => }

    try { x.remove(1); sys.error("no exception for removed element") } 
    catch { case i:IndexOutOfBoundsException => }

    x += "two2"
    assert(x.length == 2, "length D")

    // removing first element
    x.remove(0)
    assert(x.length == 1, "length E")

    // toList
    assert(x.toList == List("two2"), "toList")

    // clear
    x.clear()
    assert(x.length == 0, "length 0")
    assert(x.isEmpty, "isEmpty")

    // copyToBuffer
    x += "a"
    x += "b"
    val dest = new ArrayBuffer[String]
    x.copyToBuffer(dest)
    assert(List("a", "b") == dest.toList, "dest")
    assert(List("a", "b") == x.toList, "source")
  }

}
