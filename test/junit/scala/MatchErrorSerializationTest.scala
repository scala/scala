package scala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

/**
  * Created by estsauver on 6/15/17.
  */
@RunWith(classOf[JUnit4])
class MatchErrorSerializationTest {

  @Test
  def canSerializeMatchError = {
    import java.io._
    val matchError = new MatchError(new Object)
    val barrayOut = new ByteArrayOutputStream()
    new ObjectOutputStream(barrayOut).writeObject(matchError)
    val barrayIn = new ByteArrayInputStream(barrayOut.toByteArray)
    val readMessage = new ObjectInputStream(barrayIn).readObject().asInstanceOf[MatchError].getMessage()
    assert(readMessage.startsWith("java.lang.Object"))
  }
}
