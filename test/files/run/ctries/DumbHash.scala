





class DumbHash(val i: Int) {
  override def equals(other: Any) = other match {
    case that: DumbHash => that.i == this.i
    case _ => false
  }
  override def hashCode = i % 5
  override def toString = "DH(%s)".format(i)
}
