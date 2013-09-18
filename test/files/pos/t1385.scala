object Test extends Serializable {
  private def readResolve: AnyRef = this
}
