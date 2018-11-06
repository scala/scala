case class User()

@SerialVersionUID(13L) case class IdentifyMessage1(userName: String, user: User, code: Int)
@SerialVersionUID(10L + 3L) case class IdentifyMessage2(userName: String, user: User, code: Int)

object Test extends App {
  println("#1 " + java.io.ObjectStreamClass.lookup(IdentifyMessage1("hei", User(), 8).getClass).getSerialVersionUID)
  println("#2 " + java.io.ObjectStreamClass.lookup(IdentifyMessage2("hei", User(), 8).getClass).getSerialVersionUID)
}
