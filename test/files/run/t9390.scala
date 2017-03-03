class C {
  def methodLift1 = {
    def isEven(c: Int) = c % 2 == 0
    val f: Int => Boolean = isEven
    f
  }
  def methodLift2 = {
    def isEven(c: Int) = c % 2 == 0
    def isEven0(c: Int) = isEven(c)
    val f: Int => Boolean = isEven0
    f
  }

  def methodLift3 = {
    def isEven(c: Int) = {toString; c % 2 == 0}
    def isEven0(c: Int) = isEven(c)
    val f: Int => Boolean = isEven0
    f
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C

    {
      val f = c.methodLift1
      assert(f(0))
      assert(!f(1))
      val f1 = serializeDeserialize(f)
      assert(f1(0))
      assert(!f1(1))
    }


    {
      val f = c.methodLift2
      assert(f(0))
      assert(!f(1))
      val f1 = serializeDeserialize(f)
      assert(f1(0))
      assert(!f1(1))
    }

    {
      val f = c.methodLift3
      assert(f(0))
      assert(!f(1))
      try {
        serializeDeserialize(this)
        assert(false)
      } catch {
        case _: java.io.NotSerializableException =>
          // expected, the closure in methodLift3 must capture C which is not serializable
      }
    }
  }

  def serializeDeserialize[T <: AnyRef](obj: T): T = {
    import java.io._
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }
}
