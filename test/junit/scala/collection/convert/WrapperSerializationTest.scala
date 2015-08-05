package scala.collection.convert

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class WrapperSerializationTest {
  def ser(a: AnyRef) = {
    val baos = new java.io.ByteArrayOutputStream
    (new java.io.ObjectOutputStream(baos)).writeObject(a)
    baos
  }
  def des(baos: java.io.ByteArrayOutputStream): AnyRef = {
    val bais = new java.io.ByteArrayInputStream(baos.toByteArray)
    (new java.io.ObjectInputStream(bais)).readObject()
  }
  def serdes(a: AnyRef): Boolean = a == des(ser(a))

  @Test
  def test_SI8911() {
    import scala.collection.JavaConverters._
    assert( serdes(scala.collection.mutable.ArrayBuffer(1,2).asJava) )
    assert( serdes(Seq(1,2).asJava) )
    assert( serdes(Set(1,2).asJava) )
    assert( serdes(Map(1 -> "one", 2 -> "two").asJava) )
  }
}
