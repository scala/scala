object Test extends App {
  val demo = new Demo
  demo.x = 1
  demo.A.y = 2
  demo.B.z = 3

  val demo1 = rt(demo)
  println(s"x: ${demo1.x}")
  println(s"y: ${demo1.A.y}")
  println(s"z: ${demo1.B.z}")

  val demo2 = new Demo
  val d2s1 = ser(demo2)
  demo2.B
  val d2s2 = ser(demo2)
  println(java.util.Arrays.equals(d2s1, d2s2))

  import java.io._
  def ser(t: Serializable): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    new ObjectOutputStream(baos).writeObject(t)
    baos.toByteArray
  }
  def rt[T <: Serializable](t: T): T = {
    new ObjectInputStream(new ByteArrayInputStream(ser(t))).readObject().asInstanceOf[T]
  }
}

class Demo extends Serializable {
  var x: Int = _
  object A extends Serializable { var y: Int = _ }
  @transient object B extends Serializable { var z: Int = _ }
}
