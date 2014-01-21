/* NSC -- new Scala compiler
* Copyright 2005-2013 LAMP/EPFL
* @author  Paul Phillips
*/

package scala.tools.nsc
package typechecker

/** A generic means of breaking down types into their subcomponents.
 *  Types are decomposed top down, and recognizable substructure is
 *  dispatched via self-apparently named methods.  Those methods can
 *  be overridden for custom behavior, but only the abstract methods
 *  require implementations, each of which must create some unknown
 *  "Node" type from its inputs.
 *
 *  - wrapProduct   create Node from a product of Nodes
 *  - wrapSequence  create Node from a sequence of Nodes
 *  - wrapAtom      create Node from an arbitrary value
 *
 *  This is a work in progress.
 */
trait DestructureTypes {
  val global: Global
  import global._
  import definitions.{ NothingClass, AnyClass }

  trait DestructureType[Node] extends (Type => Node) {
    def withLabel(node: Node, label: String): Node
    def withType(node: Node, typeName: String): Node

    def wrapEmpty: Node
    def wrapPoly(in: Node, out: Node): Node
    def wrapMono(in: Node, out: Node): Node
    def wrapProduct(nodes: List[Node]): Node
    def wrapSequence(nodes: List[Node]): Node
    def wrapAtom[U](value: U): Node

    private val openSymbols = scala.collection.mutable.Set[Symbol]()

    private def nodeList[T](elems: List[T], mkNode: T => Node): Node =
      if (elems.isEmpty) wrapEmpty else list(elems map mkNode)

    private def scopeMemberList(elems: List[Symbol]): Node         = nodeList(elems, wrapAtom)
    private def typeList(elems: List[Type]): Node                  = nodeList(elems, this)
    private def symbolList(elems: List[Symbol]): Node              = nodeList(elems, wrapSymbolInfo)
    private def treeList(elems: List[Tree]): Node                  = nodeList(elems, wrapTree)
    private def annotationList(annots: List[AnnotationInfo]): Node = nodeList(annots, annotation)

    private def assocsNode(ann: AnnotationInfo): Node = {
      val (names, args) = ann.assocs.toIndexedSeq.unzip
      if (names.isEmpty) wrapEmpty
      else node("assocs", nodeList(names.indices.toList, (i: Int) => atom(names(i).toString, args(i))))
    }
    private def typeTypeName(tp: Type) = tp match {
      case mt @ MethodType(_, _) if mt.isImplicit => "ImplicitMethodType"
      case TypeRef(_, sym, _)                     => typeRefType(sym)
      case _                                      => tp.kind
    }

    def wrapTree(tree: Tree): Node = withType(
      tree match {
        case x: NameTree => atom(x.name.toString, x)
        case _           => wrapAtom(tree)
      },
      tree.productPrefix
    )
    def wrapSymbolInfo(sym: Symbol): Node = {
      if ((sym eq NoSymbol) || openSymbols(sym)) wrapEmpty
      else {
        openSymbols += sym
        try product(symbolType(sym), wrapAtom(sym.defString))
        finally openSymbols -= sym
      }
    }

    def list(nodes: List[Node]): Node = wrapSequence(nodes)
    def product(tp: Type, nodes: Node*): Node = product(typeTypeName(tp), nodes: _*)
    def product(typeName: String, nodes: Node*): Node = (
      nodes.toList filterNot (_ == wrapEmpty) match {
        case Nil => wrapEmpty
        case xs  => withType(wrapProduct(xs), typeName)
      }
    )

    def atom[U](label: String, value: U): Node         = node(label, wrapAtom(value))
    def constant(label: String, const: Constant): Node = atom(label, const)

    def scope(decls: Scope): Node          = node("decls", scopeMemberList(decls.toList))

    def resultType(restpe: Type): Node          = this("resultType", restpe)
    def typeParams(tps: List[Symbol]): Node     = node("typeParams", symbolList(tps))
    def valueParams(params: List[Symbol]): Node = node("params", symbolList(params))
    def typeArgs(tps: List[Type]): Node         = node("args", typeList(tps))
    def parentList(tps: List[Type]): Node       = node("parents", typeList(tps))

    def polyFunction(tparams: List[Symbol], restpe: Type): Node = wrapPoly(typeParams(tparams), resultType(restpe))
    def monoFunction(params: List[Symbol], restpe: Type): Node  = wrapMono(valueParams(params), resultType(restpe))
    def nullaryFunction(restpe: Type): Node                     = wrapMono(wrapEmpty, this(restpe))

    def prefix(pre: Type): Node = pre match {
      case NoPrefix => wrapEmpty
      case _        => this("pre", pre)
    }
    def typeBounds(lo0: Type, hi0: Type): Node = {
      val lo = if ((lo0 eq WildcardType) || (lo0.typeSymbol eq NothingClass)) wrapEmpty else this("lo", lo0)
      val hi = if ((hi0 eq WildcardType) || (hi0.typeSymbol eq AnyClass)) wrapEmpty else this("hi", hi0)

      product("TypeBounds", lo, hi)
    }

    def annotation(ann: AnnotationInfo): Node = product(
      "AnnotationInfo",
      this("atp", ann.atp),
      node("args", treeList(ann.args)),
      assocsNode(ann)
    )
    def typeConstraint(constr: TypeConstraint): Node = product(
      "TypeConstraint",
      node("lo", typeList(constr.loBounds)),
      node("hi", typeList(constr.hiBounds)),
      this("inst", constr.inst)
    )
    def annotatedType(annotations: List[AnnotationInfo], underlying: Type) = product(
      "AnnotatedType",
      node("annotations", annotationList(annotations)),
      this("underlying", underlying)
    )

    /** This imposes additional structure beyond that which is visible in
     *  the case class hierarchy.  In particular, (too) many different constructs
     *  are encoded in TypeRefs; here they are partitioned somewhat before
     *  being dispatched.
     *
     *  For example, a typical type parameter is encoded as TypeRef(NoPrefix, sym, Nil)
     *  with its upper and lower bounds stored in the info of the symbol.  Viewing the
     *  TypeRef naively we are treated to both too much information (useless prefix, usually
     *  empty args) and too little (bounds hidden behind indirection.) So drop the prefix
     *  and promote the bounds.
     */
    def typeRef(tp: TypeRef) = {
      val TypeRef(pre, sym, args) = tp
      // Filtered down to elements with "interesting" content
      product(
        tp,
        if (sym.isDefinedInPackage) wrapEmpty else prefix(pre),
        wrapSymbolInfo(sym),
        typeArgs(args),
        if (tp ne tp.normalize) this("normalize", tp.normalize) else wrapEmpty
      )
    }

    def symbolType(sym: Symbol) = (
      if (sym.isRefinementClass) "Refinement"
      else if (sym.isAliasType) "Alias"
      else if (sym.isTypeSkolem) "TypeSkolem"
      else if (sym.isTypeParameter) "TypeParam"
      else if (sym.isAbstractType) "AbstractType"
      else if (sym.isType) "TypeSymbol"
      else "TermSymbol"
    )
    def typeRefType(sym: Symbol) = (
      if (sym.isRefinementClass) "RefinementTypeRef"
      else if (sym.isAliasType) "AliasTypeRef"
      else if (sym.isTypeSkolem) "SkolemTypeRef"
      else if (sym.isTypeParameter) "TypeParamTypeRef"
      else if (sym.isAbstractType) "AbstractTypeRef"
      else "TypeRef"
    ) + ( if (sym.isFBounded) "(F-Bounded)" else "" )

    def node(label: String, node: Node): Node = withLabel(node, label)
    def apply(label: String, tp: Type): Node  = withLabel(this(tp), label)

    def apply(tp: Type): Node = tp match {
      case AntiPolyType(pre, targs)                  => product(tp, prefix(pre), typeArgs(targs))
      case ClassInfoType(parents, decls, clazz)      => product(tp, parentList(parents), scope(decls), wrapAtom(clazz))
      case ConstantType(const)                       => product(tp, constant("value", const))
      case OverloadedType(pre, alts)                 => product(tp, prefix(pre), node("alts", typeList(alts map pre.memberType)))
      case RefinedType(parents, decls)               => product(tp, parentList(parents), scope(decls))
      case SingleType(pre, sym)                      => product(tp, prefix(pre), wrapAtom(sym))
      case SuperType(thistp, supertp)                => product(tp, this("this", thistp), this("super", supertp))
      case ThisType(clazz)                           => product(tp, wrapAtom(clazz))
      case TypeVar(inst, constr)                     => product(tp, this("inst", inst), typeConstraint(constr))
      case AnnotatedType(annotations, underlying)    => annotatedType(annotations, underlying)
      case ExistentialType(tparams, underlying)      => polyFunction(tparams, underlying)
      case PolyType(tparams, restpe)                 => polyFunction(tparams, restpe)
      case MethodType(params, restpe)                => monoFunction(params, restpe)
      case NullaryMethodType(restpe)                 => nullaryFunction(restpe)
      case TypeBounds(lo, hi)                        => typeBounds(lo, hi)
      case tr @ TypeRef(pre, sym, args)              => typeRef(tr)
      case _                                         => wrapAtom(tp) // XXX see what this is
    }
  }
}
