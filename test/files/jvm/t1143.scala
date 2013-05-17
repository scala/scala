object Serialize {
  @throws(classOf[java.io.IOException])
  def write[A](o: A): Array[Byte] = {
    val ba = new java.io.ByteArrayOutputStream(512)
    val out = new java.io.ObjectOutputStream(ba)
    out.writeObject(o)
    out.close()
    ba.toByteArray()
  }
  @throws(classOf[java.io.IOException])
  @throws(classOf[ClassNotFoundException])
  def read[A](buffer: Array[Byte]): A = {
    val in =
      new java.io.ObjectInputStream(new java.io.ByteArrayInputStream(buffer))
    in.readObject().asInstanceOf[A]
  }
}

@SerialVersionUID(1L)
class VarModel[T](getter: => T, setter: T => Unit) extends Serializable {
  Serialize.write(getter)
  Serialize.write(setter)

  def this(getter: => T) = this(getter, null)

  def getObject: AnyRef = getter.asInstanceOf[AnyRef]

  def setObject(v: AnyRef) = {
    if (setter == null)
      throw new RuntimeException("Tried to set readonly model!")
    setter(v.asInstanceOf[T])
  }

  def detach = ()
}

@SerialVersionUID(1L)
class Printer(p: VarModel[String]) extends Serializable {
  def print = println(p.getObject)
}

@SerialVersionUID(1L)
class Component extends Serializable {
}

class Form extends Component {
}

@SerialVersionUID(1L)
class Main extends Serializable {
  var pass = "pass"
  def main(args: Array[String]) {
    val f = new Form {
      val p = new Printer(new VarModel(pass, s => pass = s))
    }
    ()
  }
}

object Test {
  def main(args: Array[String]) {
    (new Main).main(Array[String]())
  }
}
