package foo

package object bar {
  class Bippy(x: Int) {
    class Ding
    object Ding
    case class Dong(x: Float)
  }
  object Bippy {
    class Dingus
    object Dingus
    case class Dongus(x: Float)

    def apply(xs: Int*) = new Bippy(xs.sum)
    def apply() = new Bippy(5)
  }
}
