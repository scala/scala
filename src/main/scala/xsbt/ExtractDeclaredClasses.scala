package xsbt

import scala.tools.nsc._

class ExtractDeclaredClasses[GlobalType <: CallbackGlobal](val global: GlobalType) extends LocateClassFile {
  import global._

  def extract(unit: CompilationUnit): Set[String] = {
    val tree = unit.body
    val extractedByTreeWalk = extractByTreeWalk(tree)
    extractedByTreeWalk
  }

  private def extractByTreeWalk(tree: Tree): Set[String] = {
    val traverser = new DeclaredPublicClassesTraverser
    traverser.traverse(tree)
    traverser.declaredClassesBuffer.toSet
  }

  private class DeclaredPublicClassesTraverser {
    val declaredClassesBuffer = collection.mutable.ListBuffer.empty[String]
    def traverse(tree: Tree): Unit = tree match {
      case PackageDef(_, stats) => stats.foreach(traverse)
      case classLikeDef: ImplDef =>
        val classLikeSymbol = classLikeDef.symbol
        if (!classLikeSymbol.isSynthetic && !classLikeSymbol.isPrivate) {
          val className = fullName(classLikeSymbol)
          declaredClassesBuffer += className
          val body = classLikeDef.impl.body
          body.foreach(traverse)
        }
      case _ => ()
    }

    private def fullName(s: Symbol): String = className(s)
  }

}
