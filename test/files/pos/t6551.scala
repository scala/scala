import scala.language.dynamics

object Test {
  def main(args: Array[String]) {
    class Lenser[T] extends Dynamic {
      def selectDynamic(propName: String) = ???
    }

    def lens[T] = new Lenser[T]

    val qq = lens[String]
  }
}
