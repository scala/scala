package bug687;
trait Query {
  override def equals(o : Any) = false;
}
