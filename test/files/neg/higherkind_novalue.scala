abstract class HigherKind[m[s]] {
  val x: m // type of kind *->* doesn't classify a value, but a val/def/... can only contain/return a value
  def y: m
}
