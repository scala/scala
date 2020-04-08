package tastytest

object TestOpenWriter extends Suite("TestOpenWriter") {

  trait Encryptable[T] {
    def encrypt(t: T): T
  }

  implicit object EncryptString extends Encryptable[String] {
    def encrypt(t: String): String = "*" * t.length()
  }

  class EncryptedWriter[T: Encryptable] extends OpenWriter[T] {
    override def send(x: T) = super.send(implicitly[Encryptable[T]].encrypt(x))
  }

  val ew = new EncryptedWriter[String]

  test(assert(ew.send("abc") === "***"))
  test(assert(ew.sendAll("abc", "defg", "hijkl") === "***,****,*****"))

}
