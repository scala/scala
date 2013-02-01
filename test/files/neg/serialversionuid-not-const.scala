@SerialVersionUID(13l.toLong) class C1 extends Serializable
@SerialVersionUID(13l) class C2 extends Serializable
@SerialVersionUID(13.asInstanceOf[Long]) class C3 extends Serializable
@SerialVersionUID(Test.bippy) class C4 extends Serializable

object Test {
  val bippy = 13L

  def show(c: Class[_]) = println(java.io.ObjectStreamClass.lookup(c).getSerialVersionUID)
  def main(args: Array[String]): Unit = {
    show(classOf[C1])
    show(classOf[C2])
    show(classOf[C3])
    show(classOf[C4])
  }
}
