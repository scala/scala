import scala.collection.JavaConversions._

class Outer {
  protected class Inner extends BeanDefinitionVisitor {
    protected def visitMap(mapVal: Map[_, _]): Unit = ()
  }
}