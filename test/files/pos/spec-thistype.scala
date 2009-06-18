class Foo[@specialized A] {
  def bar(xs: List[A]): this.type = this
}
