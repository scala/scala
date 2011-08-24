object B
{
  def a = {
    for (n <- Nil; m <- Nil) {
      val p = badIdentifier
      false
    }
  }
}
