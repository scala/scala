import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.lang.invoke.{MethodHandleInfo, SerializedLambda}

import scala.tools.nsc.util

class C extends java.io.Serializable {
  val fs = List(
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => (),
    () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => (), () => () ,() => (), () => (), () => (), () => (), () => ()
  )
  private def foo(): Unit = {
    assert(false, "should not be called!!!")
  }
}

trait FakeSam { def apply(): Unit }

object Test {
  def main(args: Array[String]): Unit = {
    allRealLambdasRoundTrip()
    fakeLambdaFailsToDeserialize()
  }

  def allRealLambdasRoundTrip(): Unit = {
    new C().fs.map(x => serializeDeserialize(x).apply())
  }

  def fakeLambdaFailsToDeserialize(): Unit = {
    val fake = new SerializedLambda(classOf[C], classOf[FakeSam].getName, "apply", "()V",
      MethodHandleInfo.REF_invokeVirtual, classOf[C].getName, "foo", "()V", "()V", Array(new C))
    try {
      serializeDeserialize(fake).asInstanceOf[FakeSam].apply()
      assert(false)
    } catch {
      case ex: Exception =>
        val stackTrace = util.stackTraceString(ex)
        assert(stackTrace.contains("Illegal lambda deserialization"), stackTrace)
    }
  }

  def serializeDeserialize[T <: AnyRef](obj: T) = {
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }
}

