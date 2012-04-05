/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

/* Utilities for generating SourceLocations.
 * 
 * @author  Philipp Haller
 */
trait SourceLocations {
  self: Analyzer =>
  
  import global._
  import definitions._
  
  def sourceLocation(typer: Typer, infoTree: Tree): SearchResult = {
    /** Creates a tree that calls the factory method called constructor in object reflect.SourceLocation */
    def sourceLocationFactoryCall(constructor: String, args: Tree*): Tree =
      if (args contains EmptyTree) EmptyTree
      else typer.typedPos(infoTree.pos.focus) {
        Apply(
          Select(gen.mkAttributedRef(SourceLocationModule), constructor),
          args.toList
        )
      }
    
    def srcLocation()(implicit from: List[Symbol] = List(), to: List[Type] = List()): SearchResult = {
      implicit def wrapResult(tree: Tree): SearchResult =
        if (tree == EmptyTree) SearchFailure else new SearchResult(tree, new TreeTypeSubstituter(from, to))
      
      val position = infoTree.pos.focus
      val fileName = if (position.isDefined) position.source.file.absolute.path
                     else "<unknown file>"
      sourceLocationFactoryCall("apply", Literal(Constant(position.line)), Literal(Constant(position.point)), Literal(Constant(fileName)))
    }
    
    srcLocation()
  }

}
