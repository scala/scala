trait T {
  protected def s: String
}

case class G(override protected val s: String) extends T
