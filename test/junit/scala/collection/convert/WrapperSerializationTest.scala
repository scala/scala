package scala.collection.convert

import org.junit.Assert._
import org.junit.Test

import scala.jdk.CollectionConverters._

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
  @Test
  def test_SI8911(): Unit = {
    def serdes(a: AnyRef): Unit = assertEquals(a, des(ser(a)))
    serdes(scala.collection.mutable.ArrayBuffer(1,2).asJava)
    serdes(Seq(1,2).asJava)
    serdes(Set(1,2).asJava)
    serdes(Map(1 -> "one", 2 -> "two").asJava)
  }
}
