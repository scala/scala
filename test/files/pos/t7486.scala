object Test{
  var locker = 0
  // remove implicit, or change to `locker = locker + 1` to make it compile.
  implicit val davyJones0 = {
    locker += 0
    0
  }
}
