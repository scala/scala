object Test extends App {
  object Truc {
    override def toString() = "oui"
    def toString(bool: Boolean) = "chaispas"
  }
  val tata: String = Truc.toString
}
