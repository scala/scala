trait t6962 {
  def sth() = {
    crashes()
  }
  protected[Bar] def crashes(withDefaultParam: Boolean = true): Int = 42
}
