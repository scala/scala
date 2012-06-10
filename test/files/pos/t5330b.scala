abstract trait Base {
  def foo: this.type
};
class Derived[T] extends Base {
  def foo: Nothing = sys.error("!!!")
}
