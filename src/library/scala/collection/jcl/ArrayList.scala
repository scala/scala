package scala.collection.jcl;

/** Creates a buffer backed by a Java array list.
 * @author Sean McDirmid
 */
class ArrayList[A](override val underlying : java.util.ArrayList) extends ConcreteWrapper[A] with BufferWrapper[A]  {
  def this() = this(new java.util.ArrayList);
  override def elements = super[BufferWrapper].elements;
}
