trait MM {
  protected def method = "bip"
}
trait NN {
  protected def method = "bop"
}
trait OOOOO extends MM with NN {
  override protected def method = super[MM].method + super[NN].method
  override def hashCode = super[MM].hashCode + super[NN].hashCode
}
