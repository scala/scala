trait Companion[T] {
  def parse(value: String): Option[T]
  def apply(value: String): T = parse(value).get
}
