import scala.tools.nsc._  

object Test {
  
  /**
   *  Type inference overlooks constraints posed by type parameters in annotations on types.
   */
  
  val testCode = <code>
  
    class posingAs[A] extends TypeConstraint
    
    def resolve[A,B](x: A @posingAs[B]): B = x.asInstanceOf[B]
    
    val x = resolve(7: @posingAs[Any])
  
  </code>.text
  
  def main(args: Array[String]) = {
    
    val settings = new Settings()
    settings.classpath.value = System.getProperty("java.class.path")
    val tool = new Interpreter(settings)
    val global = tool.compiler

    import global._
    import definitions._

    object checker extends AnnotationChecker {

      /** Check annotations to decide whether tpe1 <:< tpe2 */
      def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {

        tpe1.annotations.forall(a1 => tpe2.annotations.forall(a2 => a1.atp <:< a2.atp))

      }
    }
    
    global.addAnnotationChecker(checker)
    
    tool.interpret(testCode)
    
  }

}

