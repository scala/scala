
package test;
object Test {
  class Editor {
    private object extraListener {
      def h : AnyRef = extraListener
    }
    def f = extraListener.h
  }
  def main(args : Array[String]) : Unit = (new Editor).f
}
