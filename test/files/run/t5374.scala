


import collection.mutable.ListBuffer
import java.io._



object Test {

  def main(args: Array[String]) {
    ticketExample()
    emptyListBuffer()
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

}
