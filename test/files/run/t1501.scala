import scala.tools.nsc._  

object Test {
  
  /**
   *  ...
   */
  
  val testCode = <code>
  
    class xyz[A] extends TypeConstraint
    
    def loopWhile[T](cond: =>Boolean)(body: =>(Unit @xyz[T])): Unit @ xyz[T] = {{
      if (cond) {{
        body
        loopWhile[T](cond)(body)
      }}
    }}

    def test() = {{
      var x = 7
      loopWhile(x != 0) {{
        x = x - 1
        (): @xyz[Int]
      }}
    }}
    
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

