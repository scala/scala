object B
{
  def a = {
    for (val n <- Nil; val m <- Nil) {
      val p = badIdentifier
      false
    }
  }
}
