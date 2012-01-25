import User.UserStatus._

class User {
  var id: Int = 0
  var email: String = null
  var password: String = null
  var userStatus: UserStatus = null
}

object User {
  object UserStatus extends Enumeration {
    type UserStatus = Value

    val Active = Value("1")
    val Disabled = Value("2")
  }
}
