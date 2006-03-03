object Test extends Application {
  object Truc {
    override def toString() = "oui"
    def toString(bool: Boolean) = "chaispas"
  }
  val tata: String = Truc.toString
}
