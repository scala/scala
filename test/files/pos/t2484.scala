import concurrent.ExecutionContext.Implicits.global

class Admin extends javax.swing.JApplet {
  val jScrollPane = new javax.swing.JScrollPane (null, 0, 0)
  def t2484: Unit = {
    scala.concurrent.Future {jScrollPane.synchronized {
      def someFunction () = {}
      //scala.concurrent.ops.spawn {someFunction ()}
      jScrollPane.addComponentListener (new java.awt.event.ComponentAdapter {override def componentShown (e: java.awt.event.ComponentEvent) = {
        someFunction (); jScrollPane.removeComponentListener (this)}})
    }}
  }
}
// t2630.scala
object Test {
  def meh(xs: List[Any]) {
    xs map { x =>  (new AnyRef {}) }
  }
}
