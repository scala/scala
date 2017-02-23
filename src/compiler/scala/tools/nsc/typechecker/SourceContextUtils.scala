/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

/* Utilities for generating SourceLocations and SourceContexts.
 * 
 * @author  Philipp Haller
 */
trait SourceContextUtils {
  self: Analyzer =>

  import global._
  import definitions._

  def contextSourceInfoChain(ctx: Context,
                             stopAt: Context,
                             prevValDef: Option[String]): List[(String, Int)] = {
    if (ctx == stopAt)
      List()
    else ctx.tree match {
      case vd @ ValDef(_, name, _, _) if prevValDef.isEmpty || (!prevValDef.get.equals(name.toString)) =>
        (name.toString, vd.pos.line) :: contextSourceInfoChain(ctx.outer, stopAt, Some(name.toString))
      //case app @ Apply(fun, args) if fun.symbol.isMethod =>
      //  (fun.symbol.nameString, fun.pos.line) :: contextSourceInfoChain(ctx.outer, stopAt)
      case _ =>
        contextSourceInfoChain(ctx.outer, stopAt, None)
    }
  }

  def contextInfoChain(ctx: Context, tree: Tree) = ctx.tree match {
    case vd @ ValDef(_, name, _, _) =>
      //println("current context tree is ValDef "+name)
      contextSourceInfoChain(ctx, ctx.enclClass, None)
    case _ =>
      //println("current context tree: "+ctx.tree)
      val l = tree.pos match {
        case NoPosition => 0
        case _ => tree.pos.line
      }
    (null, l) :: contextSourceInfoChain(ctx.outer, ctx.outer.enclClass, None)
  }

  def sourceInfoTree(chain: List[(String, Int)]): Tree = chain match {
    case (name, line) :: rest =>
      val pairTree = gen.mkTuple(List(Literal(Constant(name)), Literal(Constant(line))))
      Apply(Select(gen.mkAttributedRef(ListModule), nme.apply), List(pairTree))
    case List() =>
      gen.mkNil
  }

  /** Creates a tree that calls the factory method called constructor in object reflect.SourceContext */
  def sourceInfoFactoryCall(typer: Typer, infoTree: Tree, constructor: String, args: Tree*): Tree =
    if (args contains EmptyTree) EmptyTree
    else typer.typedPos(infoTree.pos.focus) {
      Apply(
        Select(gen.mkAttributedRef(SourceContextModule), constructor),
        args.toList
      )
    }

  def methodNameOf(tree: Tree) = {
    tree match {
      case Apply(TypeApply(s, _), _) => s.symbol.name
      case Apply(s@Select(_, _), _) => s.symbol.name
      case Apply(s@Ident(_), _) => s.symbol.name
      case Apply(Apply(s, _), _) => s.symbol.name
      case s@Select(_, _) => s.symbol.name
      case other => ""
    }
  }

  def receiverOptOf(tree: Tree) = {
    try {
      tree match {
        case Apply(TypeApply(Select(recv, _), _), _) => Some(recv.symbol.name)
        case Apply(Select(recv, _), _) => Some(recv.symbol.name)
        case Select(recv, _) => Some(recv.symbol.name)
        case _ => None
      }
    } catch {
      case npe: NullPointerException =>
        None
    }
  }

  def sourceInfo(typer: Typer, infoContext: Context, infoTree: Tree): SearchResult = {
    def srcInfo()(implicit from: List[Symbol] = List(), to: List[Type] = List()): SearchResult = {
      implicit def wrapResult(tree: Tree): SearchResult =
        if (tree == EmptyTree) SearchFailure else new SearchResult(tree, new TreeTypeSubstituter(from, to))
      
      val methodName = methodNameOf(infoTree)
      val receiver =   receiverOptOf(infoTree)

      //println("context source info chain:")
      //println(contextInfoChain)
      //println("source info tree:")
      //println(sourceInfoTree(contextInfoChain))

      val position = infoTree.pos.focus
      val fileName = if (position.isDefined) position.source.file.absolute.path
                       else "<unknown file>"
      if (receiver.isEmpty)
        sourceInfoFactoryCall(typer, infoTree, "apply", Literal(Constant(fileName)), Literal(Constant(methodName.toString)), sourceInfoTree(contextInfoChain(infoContext, infoTree)))
      else
        sourceInfoFactoryCall(typer, infoTree, "apply", Literal(Constant(fileName)), Literal(Constant(methodName.toString)), Literal(Constant(receiver.get.toString)), sourceInfoTree(contextInfoChain(infoContext, infoTree)))
    }

    srcInfo()
  }

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

  /** Update search result, so that it has the correct source position.
   *  Invoke `update` on SourceContexts for chaining.
   */
  def updatedWithSourceContext(typer: Typer, tree: Tree, pt: Type, context: Context, previous: SearchResult, updateSourceContext: Boolean): SearchResult = pt/*.dealias*/ match {
    case TypeRef(_, SourceContextClass, _) if updateSourceContext =>
      val position = tree.pos.focus
      val fileName = if (position.isDefined) position.source.file.absolute.path
                     else "<unknown file>"
      val methodName = methodNameOf(tree)
      val receiver =   receiverOptOf(tree)
      new SearchResult(typer.typedPos(position) {
        // use sourceInfoFactoryCall to construct SourceContext
        val factoryCall = if (receiver.isEmpty)
          sourceInfoFactoryCall(typer, tree, "apply", Literal(Constant(fileName)), Literal(Constant(methodName.toString)), sourceInfoTree(contextInfoChain(context, tree)))
        else
          sourceInfoFactoryCall(typer, tree, "apply", Literal(Constant(fileName)), Literal(Constant(methodName.toString)), Literal(Constant(receiver.get.toString)), sourceInfoTree(contextInfoChain(context, tree)))
        Apply(Select(previous.tree, "update"), List(factoryCall))
      }, previous.subst)
    case TypeRef(_, SourceLocationClass, _) =>
      val position = tree.pos.focus
      new SearchResult(typer.typedPos(position) {
        sourceLocation(typer, tree).tree
      }, previous.subst)
    case _ => previous
  }

}
