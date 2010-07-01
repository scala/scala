package scala.swing

/*object Font {  
  def apply(fontFormat: Int, fontFile: java.io.File) = java.awt.Font.createFont(fontFormat, fontFile) 
  def apply(fontFormat: Int, fontStream: java.io.InputStream) = java.awt.Font.createFont(fontFormat, fontStream) 
  def decode(str: String) = java.awt.Font.decode(str)
  
  /* TODO: finish implementation
  /**
   * See [java.awt.Font.getFont].
   */
  def get(attributes: Map[_ <: java.text.AttributedCharacterIterator.Attribute, _]) = 
    java.awt.Font.getFont(ImmutableMapWrapper(attributes))
    
  import java.{util => ju}
  private case class ImmutableMapWrapper[A, B](underlying : Map[A, B])(m : ClassManifest[A]) extends ju.AbstractMap[A, B] {
    self =>
    override def size = underlying.size

    override def put(k : A, v : B) = 
      throw new UnsupportedOperationException("This is a wrapper that does not support mutation")
    override def remove(k : AnyRef) = 
      throw new UnsupportedOperationException("This is a wrapper that does not support mutation")
    
    override def entrySet : ju.Set[ju.Map.Entry[A, B]] = new ju.AbstractSet[ju.Map.Entry[A, B]] {
      def size = self.size

      def iterator = new ju.Iterator[ju.Map.Entry[A, B]] {
        val ui = underlying.iterator
        var prev : Option[A] = None
        
        def hasNext = ui.hasNext
      
        def next = {
          val (k, v) = ui.next
          prev = Some(k)
          new ju.Map.Entry[A, B] {
            def getKey = k
            def getValue = v
            def setValue(v1 : B) = self.put(k, v1)
            override def equals(other : Any) = other match {
              case e : ju.Map.Entry[_, _] => k == e.getKey && v == e.getValue
              case _ => false
            }
          }
        }
        
        def remove = prev match {
          case Some(k) => val v = self.remove(k.asInstanceOf[AnyRef]) ; prev = None ; v
          case _ => throw new IllegalStateException("next must be called at least once before remove")
        }
      }
    }
  }
  */
  
  /**
   * See [java.awt.Font.getFont].
   */
  def get(nm: String) = java.awt.Font.getFont(nm)
  /**
   * See [java.awt.Font.getFont].
   */
  def get(nm: String, font: Font) = java.awt.Font.getFont(nm, font)
  
  def Insets(x: Int, y: Int, width: Int, height: Int) = new Insets(x, y, width, height)
  def Rectangle(x: Int, y: Int, width: Int, height: Int) = new Insets(x, y, width, height)
  def Point(x: Int, y: Int) = new Point(x, y)
  def Dimension(x: Int, y: Int) = new Dimension(x, y) 
}*/