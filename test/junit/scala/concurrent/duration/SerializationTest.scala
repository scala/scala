package scala.concurrent.duration

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test


@RunWith(classOf[JUnit4])
class SerializationTest {
  @Test
  def test_SI9197 {
    def ser(a: AnyRef): Array[Byte] = {
      val bais = new java.io.ByteArrayOutputStream
      (new java.io.ObjectOutputStream(bais)).writeObject(a)
      bais.toByteArray
    }
    def des(ab: Array[Byte]): AnyRef =
      (new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(ab))).readObject
    
    assert(Duration.Undefined eq des(ser(Duration.Undefined)))
    assert(Duration.Inf eq des(ser(Duration.Inf)))
    assert(Duration.MinusInf eq des(ser(Duration.MinusInf)))
  }
}
