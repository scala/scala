package scala

import org.junit.jupiter.api.Test

/**
  * Created by estsauver on 6/15/17.
  */
class MatchErrorSerializationTest {

  @Test
  def canSerializeMatchError(): Unit = {
    import java.io._
    val matchError = new MatchError(new Object)
    val barrayOut = new ByteArrayOutputStream()
    new ObjectOutputStream(barrayOut).writeObject(matchError)
    val barrayIn = new ByteArrayInputStream(barrayOut.toByteArray)
    val readMessage = new ObjectInputStream(barrayIn).readObject().asInstanceOf[MatchError].getMessage()
    assert(readMessage.startsWith("java.lang.Object"))
  }
}
