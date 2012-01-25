


import collection.mutable.ListBuffer
import java.io._



object Test {
  
  def main(args: Array[String]) {
    ticketExample()
    emptyListBuffer()
    list()
    legacyList()
    objectWithMultipleLists()
  }
  
  def inAndOut[T <: AnyRef](obj: T): T = {
    val baos = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(baos)
    oos.writeObject( obj )
    val bais = new ByteArrayInputStream( baos.toByteArray )
    val ois = new ObjectInputStream(bais)
    ois.readObject.asInstanceOf[T]
  }
  
  def ticketExample() {
    val lb = inAndOut(ListBuffer(1, 2, 3))
    val lb2 = ListBuffer[Int]() ++= lb
    
    lb2 ++= List(1)
    lb ++= List(1)
    println(lb)
    println(lb2)
  }
  
  def emptyListBuffer() {
    val lb = inAndOut(ListBuffer[Int]())
    
    println(lb)
  }
  
  def list() {
    val l = inAndOut(List(1, 2, 3, 4, 5))
    
    println(l)
  }
  
  // this byte array corresponds to what List(1, 2, 3) used to be serialized to prior to this fix
  val listBytes = Array[Byte](-84, -19, 0, 5, 115, 114, 0, 39, 115, 99, 97, 108, 97, 46, 99, 111, 108, 108, 101, 99, 116, 105, 111, 110, 46, 105, 109, 109, 117, 116, 97, 98, 108, 101, 46, 36, 99, 111, 108, 111, 110, 36, 99, 111, 108, 111, 110, -118, 92, 99, 91, -10, -40, -7, 109, 3, 0, 2, 76, 0, 43, 115, 99, 97, 108, 97, 36, 99, 111, 108, 108, 101, 99, 116, 105, 111, 110, 36, 105, 109, 109, 117, 116, 97, 98, 108, 101, 36, 36, 99, 111, 108, 111, 110, 36, 99, 111, 108, 111, 110, 36, 36, 104, 100, 116, 0, 18, 76, 106, 97, 118, 97, 47, 108, 97, 110, 103, 47, 79, 98, 106, 101, 99, 116, 59, 76, 0, 2, 116, 108, 116, 0, 33, 76, 115, 99, 97, 108, 97, 47, 99, 111, 108, 108, 101, 99, 116, 105, 111, 110, 47, 105, 109, 109, 117, 116, 97, 98, 108, 101, 47, 76, 105, 115, 116, 59, 120, 112, 115, 114, 0, 17, 106, 97, 118, 97, 46, 108, 97, 110, 103, 46, 73, 110, 116, 101, 103, 101, 114, 18, -30, -96, -92, -9, -127, -121, 56, 2, 0, 1, 73, 0, 5, 118, 97, 108, 117, 101, 120, 114, 0, 16, 106, 97, 118, 97, 46, 108, 97, 110, 103, 46, 78, 117, 109, 98, 101, 114, -122, -84, -107, 29, 11, -108, -32, -117, 2, 0, 0, 120, 112, 0, 0, 0, 1, 115, 113, 0, 126, 0, 4, 0, 0, 0, 2, 115, 113, 0, 126, 0, 4, 0, 0, 0, 3, 115, 114, 0, 44, 115, 99, 97, 108, 97, 46, 99, 111, 108, 108, 101, 99, 116, 105, 111, 110, 46, 105, 109, 109, 117, 116, 97, 98, 108, 101, 46, 76, 105, 115, 116, 83, 101, 114, 105, 97, 108, 105, 122, 101, 69, 110, 100, 36, -118, 92, 99, 91, -9, 83, 11, 109, 2, 0, 0, 120, 112, 120)
  
  def legacyList() {
    val bais = new ByteArrayInputStream(listBytes)
    val ois = new ObjectInputStream(bais)
    val l = ois.readObject()
    
    println(l)
  }
  
  class Foo extends Serializable {
    val head = List(1, 2, 3)
    val last = head.tail.tail
    def structuralSharing: Boolean = head.tail.tail eq last
    
    assert(structuralSharing)
  }
  
  def objectWithMultipleLists() {
    val foo = inAndOut(new Foo)
    
    if (foo.structuralSharing) println("ok")
    else println("no structural sharing")
  }
  
}
