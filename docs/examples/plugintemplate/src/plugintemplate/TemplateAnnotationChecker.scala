package plugintemplate

import scala.tools.nsc.Global

abstract class TemplateAnnotationChecker {
  val global: Global
  import global._

  object checker extends AnnotationChecker {
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      println("checking: "+ tpe1 +" <: "+ tpe2)
      true
    }

    override def addAnnotations(tree: Tree, tpe: Type): Type = {
      println("adding annot to "+ tree.symbol)
      tpe
    }
  }
}
