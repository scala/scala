object Example {
  val badInters1 = s"foo \unope that's wrong"
  val badIntersEnd1 = s"foo \u12"
  val badInterRaw1 = raw"foo \unope that's wrong"
  val badInterRawEnd1 = raw"foo \u12"
  val badInters3 = s"""foo \unope that's wrong""" 
  val badInterRaw3 = raw"""foo \unope that's wrong"""
}