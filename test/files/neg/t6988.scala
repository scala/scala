case class User()

@SerialVersionUID(13.asInstanceOf[Long]) case class IdentifyMessage1(userName: String, user: User, code: Int)
@SerialVersionUID(13l) case class IdentifyMessage2(userName: String, user: User, code: Int)
object O {
  val SerialUID = "13".toLong
}
@SerialVersionUID(O.SerialUID) case class IdentifyMessage3(userName: String, user: User, code: Int)


