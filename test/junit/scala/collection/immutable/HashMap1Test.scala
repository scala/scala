package scala.collection.immutable

import org.junit.Assert.assertEquals
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import java.io.{ByteArrayOutputStream, ObjectOutputStream, ByteArrayInputStream, ObjectInputStream}

@RunWith(classOf[JUnit4])
class HashMap1Test {

  @Test
  def deserializesCorrectly() {
  	val kv = (1->1)
    val map = new HashMap.HashMap1(kv._1,kv._1.hashCode,kv._2,kv)

 	val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(map)
    oos.close()
    
    val bais = new ByteArrayInputStream(baos.toByteArray())
    val ois = new ObjectInputStream(bais)
    val deserializedMap = ois.readObject().asInstanceOf[HashMap$HashMap1]
    ois.close()
    
    assertEquals(kv, deserializedMap.kv)
  }
}
