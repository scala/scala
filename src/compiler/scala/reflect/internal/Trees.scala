/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import Flags._
import api.Modifier

trait Trees extends api.Trees { self: SymbolTable =>

  // --- modifiers implementation ---------------------------------------

  /** @param privateWithin the qualifier for a private (a type name)
   *    or tpnme.EMPTY, if none is given.
   *  @param annotations the annotations for the definition.
   *    '''Note:''' the typechecker drops these annotations,
   *    use the AnnotationInfo's (Symbol.annotations) in later phases.
   */
  case class Modifiers(flags: Long,
                       privateWithin: Name,
                       annotations: List[Tree]) extends AbsModifiers with HasFlags {

    var positions: Map[Long, Position] = Map()

    def setPositions(poss: Map[Long, Position]): this.type = {
      positions = poss; this
    }

    /* Abstract types from HasFlags. */
    type FlagsType          = Long
    type AccessBoundaryType = Name
    type AnnotationType     = Tree

    def hasAnnotationNamed(name: TypeName) = {
      annotations exists {
        case Apply(Select(New(Ident(`name`)), _), _)     => true
        case Apply(Select(New(Select(_, `name`)), _), _) => true
        case _                                           => false
      }
    }

    def hasAccessBoundary = privateWithin != tpnme.EMPTY
    def hasAllFlags(mask: Long): Boolean = (flags & mask) == mask
    def hasFlag(flag: Long) = (flag & flags) != 0L
    def hasFlagsToString(mask: Long): String = flagsToString(
      flags & mask,
      if (hasAccessBoundary) privateWithin.toString else ""
    )
    def defaultFlagString = hasFlagsToString(-1L)
    def & (flag: Long): Modifiers = {
      val flags1 = flags & flag
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations) setPositions positions
    }
    def &~ (flag: Long): Modifiers = {
      val flags1 = flags & (~flag)
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations) setPositions positions
    }
    def | (flag: Long): Modifiers = {
      val flags1 = flags | flag
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations) setPositions positions
    }
    def withAnnotations(annots: List[Tree]) =
      if (annots.isEmpty) this
      else copy(annotations = annotations ::: annots) setPositions positions

    def withPosition(flag: Long, position: Position) =
      copy() setPositions positions + (flag -> position)

    override def hasModifier(mod: Modifier.Value) =
      hasFlag(flagOfModifier(mod))
    override def allModifiers: Set[Modifier.Value] =
      Modifier.values filter hasModifier
    override def mapAnnotations(f: List[Tree] => List[Tree]): Modifiers =
      Modifiers(flags, privateWithin, f(annotations)) setPositions positions

    override def toString = "Modifiers(%s, %s, %s)".format(defaultFlagString, annotations mkString ", ", positions)
  }

  def Modifiers(flags: Long, privateWithin: Name): Modifiers = Modifiers(flags, privateWithin, List())
  def Modifiers(flags: Long): Modifiers = Modifiers(flags, tpnme.EMPTY)

  def Modifiers(mods: Set[Modifier.Value],
                privateWithin: Name,
                annotations: List[Tree]): Modifiers = {
    val flagSet = mods map flagOfModifier
    Modifiers((0L /: flagSet)(_ | _), privateWithin, annotations)
  }

  lazy val NoMods = Modifiers(0)

  // --- extension methods --------------------------------------------------------

  implicit def treeOps(tree: Tree): TreeOps = new TreeOps(tree)

  class TreeOps(tree: Tree) {
    def isErroneous = (tree.tpe ne null) && tree.tpe.isErroneous
    def isTyped     = (tree.tpe ne null) && !tree.tpe.isErroneous

    /** Sets the tree's type to the result of the given function.
     *  If the type is null, it remains null - the function is not called.
     */
    def modifyType(f: Type => Type): Tree =
      if (tree.tpe eq null) tree
      else tree setType f(tree.tpe)

    /** If `pf` is defined for a given subtree, call super.traverse(pf(tree)),
     *  otherwise super.traverse(tree).
     */
    def foreachPartial(pf: PartialFunction[Tree, Tree]) {
      new ForeachPartialTreeTraverser(pf).traverse(tree)
    }

    def changeOwner(pairs: (Symbol, Symbol)*): Tree = {
      pairs.foldLeft(tree) { case (t, (oldOwner, newOwner)) =>
        new ChangeOwnerTraverser(oldOwner, newOwner) apply t
      }
    }
    
    def substTreeSyms(pairs: (Symbol, Symbol)*): Tree = {
      val list  = pairs.toList
      val subst = new TreeSymSubstituter(list map (_._1), list map (_._2))
      subst(tree)
    }
    def shallowDuplicate: Tree = new ShallowDuplicator(tree) transform tree
    def shortClass: String = tree.getClass.getName split "[.$]" last

    def isErrorTyped = (tree.tpe ne null) && tree.tpe.isError

    /** When you want to know a little more than the class, but a lot
     *  less than the whole tree.
     */
    def summaryString: String = tree match {
      case Select(qual, name) => qual.summaryString + "." + name.decode
      case Ident(name)        => name.longString
      case Literal(const)     => "Literal(" + const + ")"
      case t: DefTree         => t.shortClass + " `" + t.name.decode + "`"
      case t: RefTree         => t.shortClass + " `" + t.name.longString + "`"
      case t                  =>
        t.shortClass + (
          if (t.symbol != null && t.symbol != NoSymbol) " " + t.symbol
          else ""
        )
    }
  }

  // ---- values and creators ---------------------------------------

  /** @param sym       the class symbol
   *  @return          the implementation template
   */
  def ClassDef(sym: Symbol, impl: Template): ClassDef =
    atPos(sym.pos) {
      ClassDef(Modifiers(sym.flags),
               sym.name.toTypeName,
               sym.typeParams map TypeDef,
               impl) setSymbol sym
    }

  /**
   *  @param sym       the class symbol
   *  @param impl      the implementation template
   */
  def ModuleDef(sym: Symbol, impl: Template): ModuleDef =
    atPos(sym.pos) {
      ModuleDef(Modifiers(sym.flags), sym.name.toTermName, impl) setSymbol sym
    }

  def ValDef(sym: Symbol, rhs: Tree): ValDef =
    atPos(sym.pos) {
      ValDef(Modifiers(sym.flags), sym.name.toTermName,
             TypeTree(sym.tpe) setPos focusPos(sym.pos),
             rhs) setSymbol sym
    }

  def ValDef(sym: Symbol): ValDef = ValDef(sym, EmptyTree)

  object emptyValDef extends ValDef(Modifiers(PRIVATE), nme.WILDCARD, TypeTree(NoType), EmptyTree) {
    override def isEmpty = true
    super.setPos(NoPosition)
    override def setPos(pos: Position) = { assert(false); this }
  }

  def DefDef(sym: Symbol, mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree): DefDef =
    atPos(sym.pos) {
      assert(sym != NoSymbol)
      DefDef(Modifiers(sym.flags),
             sym.name.toTermName,
             sym.typeParams map TypeDef,
             vparamss,
             TypeTree(sym.tpe.finalResultType) setPos focusPos(sym.pos),
             rhs) setSymbol sym
    }

  def DefDef(sym: Symbol, vparamss: List[List[ValDef]], rhs: Tree): DefDef =
    DefDef(sym, Modifiers(sym.flags), vparamss, rhs)

  def DefDef(sym: Symbol, mods: Modifiers, rhs: Tree): DefDef =
    DefDef(sym, mods, mapParamss(sym)(ValDef), rhs)

  def DefDef(sym: Symbol, rhs: Tree): DefDef =
    DefDef(sym, Modifiers(sym.flags), rhs)

  def DefDef(sym: Symbol, rhs: List[List[Symbol]] => Tree): DefDef =
    DefDef(sym, rhs(sym.info.paramss))

  /** A TypeDef node which defines given `sym` with given tight hand side `rhs`. */
  def TypeDef(sym: Symbol, rhs: Tree): TypeDef =
    atPos(sym.pos) {
      TypeDef(Modifiers(sym.flags), sym.name.toTypeName, sym.typeParams map TypeDef, rhs) setSymbol sym
    }

  /** A TypeDef node which defines abstract type or type parameter for given `sym` */
  def TypeDef(sym: Symbol): TypeDef =
    TypeDef(sym, TypeBoundsTree(TypeTree(sym.info.bounds.lo), TypeTree(sym.info.bounds.hi)))

  def LabelDef(sym: Symbol, params: List[Symbol], rhs: Tree): LabelDef =
    atPos(sym.pos) {
      LabelDef(sym.name.toTermName, params map Ident, rhs) setSymbol sym
    }


  /** casedef shorthand */
  def CaseDef(pat: Tree, body: Tree): CaseDef = CaseDef(pat, EmptyTree, body)

  def Bind(sym: Symbol, body: Tree): Bind =
    Bind(sym.name, body) setSymbol sym


  /** Factory method for object creation `new tpt(args_1)...(args_n)`
   *  A `New(t, as)` is expanded to: `(new t).<init>(as)`
   */
  def New(tpt: Tree, argss: List[List[Tree]]): Tree = {
    assert(!argss.isEmpty)
    val superRef: Tree = Select(New(tpt), nme.CONSTRUCTOR)
    (superRef /: argss) (Apply)
  }
  /** 0-1 argument list new, based on a symbol.
   */
  def New(sym: Symbol, args: Tree*): Tree =
    if (args.isEmpty) New(TypeTree(sym.tpe))
    else New(TypeTree(sym.tpe), List(args.toList))

  def Apply(sym: Symbol, args: Tree*): Tree =
    Apply(Ident(sym), args.toList)

  def Super(sym: Symbol, mix: TypeName): Tree = Super(This(sym), mix)

  def This(sym: Symbol): Tree = This(sym.name.toTypeName) setSymbol sym

  /** Block factory that flattens directly nested blocks.
   */
  def Block(stats: Tree*): Block = {
    if (stats.isEmpty) Block(Nil, Literal(Constant(())))
    else stats match {
      case Seq(b @ Block(_, _)) => b
      case Seq(stat) => Block(stats.toList, Literal(Constant(())))
      case Seq(_, rest @ _*) => Block(stats.init.toList, stats.last)
    }
  }

  // --- specific traversers and transformers

  protected[scala] def duplicateTree(tree: Tree): Tree = duplicator transform tree

  private lazy val duplicator = new Transformer {
    override val treeCopy = newStrictTreeCopier
    override def transform(t: Tree) = {
      val t1 = super.transform(t)
      if ((t1 ne t) && isRangePos(t1.pos)) t1 setPos focusPos(t.pos)
      t1
    }
  }

  private object posAssigner extends Traverser {
    var pos: Position = _
    override def traverse(t: Tree) {
      if (t != EmptyTree && t.pos == NoPosition) {
        t.setPos(pos)
        super.traverse(t) // TODO: bug? shouldn't the traverse be outside of the if?
      }
    }
  }

  def atPos[T <: Tree](pos: Position)(tree: T): T = {
    posAssigner.pos = pos
    posAssigner.traverse(tree)
    tree
  }

  class ForeachPartialTreeTraverser(pf: PartialFunction[Tree, Tree]) extends Traverser {
    override def traverse(tree: Tree) {
      val t = if (pf isDefinedAt tree) pf(tree) else tree
      super.traverse(t)
    }
  }

  class ChangeOwnerTraverser(val oldowner: Symbol, val newowner: Symbol) extends Traverser {
    def changeOwner(tree: Tree) = {
      if ((tree.isDef || tree.isInstanceOf[Function]) &&
          tree.symbol != NoSymbol && tree.symbol.owner == oldowner)
        tree.symbol.owner = newowner
    }
    override def traverse(tree: Tree) {
      changeOwner(tree)
      super.traverse(tree)
    }
  }

  private class ShallowDuplicator(orig: Tree) extends Transformer {
    override val treeCopy = newStrictTreeCopier
    override def transform(tree: Tree) =
      if (tree eq orig) super.transform(tree)
      else tree
  }
  // Create a readable string describing a substitution.
  private def substituterString(fromStr: String, toStr: String, from: List[Any], to: List[Any]): String = {
    "subst[%s, %s](%s)".format(fromStr, toStr, (from, to).zipped map (_ + " -> " + _) mkString ", ")
  }

  // NOTE: if symbols in `from` occur multiple times in the `tree` passed to `transform`,
  // the resulting Tree will be a graph, not a tree... this breaks all sorts of stuff,
  // notably concerning the mutable aspects of Trees (such as setting their .tpe)
  class TreeSubstituter(from: List[Symbol], to: List[Tree]) extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(_) =>
        def subst(from: List[Symbol], to: List[Tree]): Tree =
          if (from.isEmpty) tree
          else if (tree.symbol == from.head) to.head.shallowDuplicate // TODO: does it ever make sense *not* to perform a shallowDuplicate on `to.head`?
          else subst(from.tail, to.tail);
        subst(from, to)
      case _ =>
        super.transform(tree)
    }
    override def toString = substituterString("Symbol", "Tree", from, to)
  }

  class TypeMapTreeSubstituter(val typeMap: TypeMap) extends Traverser {
    override def traverse(tree: Tree) {
      if (tree.tpe ne null)
        tree.tpe = typeMap(tree.tpe)
      if (tree.isDef)
        tree.symbol modifyInfo typeMap

      super.traverse(tree)
    }
    override def apply[T <: Tree](tree: T): T = super.apply(tree.duplicate)
  }

  class TreeTypeSubstituter(val from: List[Symbol], val to: List[Type]) extends TypeMapTreeSubstituter(new SubstTypeMap(from, to)) {
    def isEmpty = from.isEmpty && to.isEmpty
    override def toString() = "TreeTypeSubstituter("+from+","+to+")"
  }

  lazy val EmptyTreeTypeSubstituter = new TreeTypeSubstituter(List(), List())

  class TreeSymSubstTraverser(val from: List[Symbol], val to: List[Symbol]) extends TypeMapTreeSubstituter(new SubstSymMap(from, to)) {
    override def toString() = "TreeSymSubstTraverser/" + substituterString("Symbol", "Symbol", from, to)
  }

  /** Substitute symbols in `from` with symbols in `to`. Returns a new
   *  tree using the new symbols and whose Ident and Select nodes are
   *  name-consistent with the new symbols.
   */
  class TreeSymSubstituter(from: List[Symbol], to: List[Symbol]) extends Transformer {
    val symSubst = new SubstSymMap(from, to)
    override def transform(tree: Tree): Tree = {
      def subst(from: List[Symbol], to: List[Symbol]) {
        if (!from.isEmpty)
          if (tree.symbol == from.head) tree setSymbol to.head
          else subst(from.tail, to.tail)
      }

      if (tree.tpe ne null) tree.tpe = symSubst(tree.tpe)
      if (tree.hasSymbol) {
        subst(from, to)
        tree match {
          case Ident(name0) if tree.symbol != NoSymbol =>
            treeCopy.Ident(tree, tree.symbol.name)
          case Select(qual, name0) =>
            treeCopy.Select(tree, transform(qual), tree.symbol.name)
          case _ =>
            super.transform(tree)
        }
      } else
        super.transform(tree)
    }
    def apply[T <: Tree](tree: T): T = transform(tree).asInstanceOf[T]
    override def toString() = "TreeSymSubstituter/" + substituterString("Symbol", "Symbol", from, to)
  }
}

