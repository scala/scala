import concurrent.ExecutionContext.Implicits.global

// this test used to use JApplet, which has ceased to exist
// in JDK 26. I've substituted JFrame without checking to see whether
// the original issue still reproduces on Scala 2.8 with the change, :shrug:
class Admin extends javax.swing.JFrame {
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
  def meh(xs: List[Any]): Unit = {
    xs map { x =>  (new AnyRef {}) }
  }
}
