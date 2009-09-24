package bug687;
trait Query {
  override def equals(o : Object) = false;
}
