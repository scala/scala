


import collection.mutable.ListBuffer
import java.io._



object Test {
  
  def main(args: Array[String]) {
    ticketExample()
    emptyListBuffer()
  }
  
  def ticketExample() {
    val baos = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(baos)
    oos.writeObject( ListBuffer(1,2,3) )
    val bais = new ByteArrayInputStream( baos.toByteArray )
    val ois = new ObjectInputStream(bais)
    val lb = ois.readObject.asInstanceOf[ListBuffer[Int]]
    val lb2 = ListBuffer[Int]() ++= lb
    
    lb2 ++= List(1)
    lb ++= List(1)
    println(lb)
    println(lb2)
  }
  
  def emptyListBuffer() {
    val baos = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(baos)
    oos.writeObject( ListBuffer() )
    val bais = new ByteArrayInputStream( baos.toByteArray )
    val ois = new ObjectInputStream(bais)
    val lb = ois.readObject.asInstanceOf[ListBuffer[Int]]
    
    println(lb)
  }
  
}
