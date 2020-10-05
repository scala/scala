//No special error message for multiple parameters, it's not likely
//that the user was trying to use a self-type
trait SelfFirst { (this: Int, that: Int) =>
}

trait SelfSecond { (that: Int, this: Int) =>
}
