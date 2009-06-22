/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.typechecker

import symtab.Flags._
import scala.collection.mutable.ListBuffer

/** <ul>
 *    <li>
 *      <code>productArity</code>, <code>element</code> implementations added
 *      to case classes
 *    </li>
 *    <li>
 *      <code>equals</code>, <code>hashCode</code> and </code>toString</code>
 *      methods are added to case classes, unless they are defined in the
 *      class or a baseclass different from <code>java.lang.Object</code>
 *    </li>
 *    <li>
 *      <code>toString</code> method is added to case objects, unless they
 *      are defined in the class or a baseclass different from
 *      <code>java.lang.Object</code>
 *    </li>
 *  </ul>
 */
trait SyntheticMethods { self: Analyzer =>
  import global._                  // the global environment
  import definitions._             // standard classes and methods
  //import global.typer.{typed}      // methods to type trees
  // @S: type hack: by default, we are used from global.analyzer context
  // so this cast won't fail. If we aren't in global.analyzer, we have
  // to override this method anyways.
  protected def typer : Typer = global.typer.asInstanceOf[Typer]

  /**
   *  @param templ ...
   *  @param clazz ...
   *  @param unit  ...
   *  @return      ...
   */
  def addSyntheticMethods(templ: Template, clazz: Symbol, context: Context): Template = try {

    val localContext = if (reporter.hasErrors) context.makeSilent(false) else context
    val localTyper = newTyper(localContext)

    def hasImplementation(name: Name): Boolean = {
      val sym = clazz.info.member(name) // member and not nonPrivateMember: bug #1385
      sym.isTerm && !(sym hasFlag DEFERRED)
    }

    def hasOverridingImplementation(meth: Symbol): Boolean = {
      val sym = clazz.info.nonPrivateMember(meth.name)
      sym.alternatives exists { sym =>
        sym != meth && !(sym hasFlag DEFERRED) && !(sym hasFlag (SYNTHETIC | SYNTHETICMETH)) &&
        (clazz.thisType.memberType(sym) matches clazz.thisType.memberType(meth))
      }
    }

    def syntheticMethod(name: Name, flags: Int, tpeCons: Symbol => Type) =
      newSyntheticMethod(name, flags | OVERRIDE, tpeCons)

    def newSyntheticMethod(name: Name, flags: Int, tpeCons: Symbol => Type) = {
      var method = clazz.newMethod(clazz.pos, name)
        .setFlag(flags | SYNTHETICMETH)
      method.setInfo(tpeCons(method))
      method = clazz.info.decls.enter(method).asInstanceOf[TermSymbol]
      method
    }

    /*
    def productSelectorMethod(n: int, accessor: Symbol): Tree = {
      val method = syntheticMethod(newTermName("_"+n), FINAL, accessor.tpe)
      typed(DefDef(method, vparamss => gen.mkAttributedRef(accessor)))
    }
    */
    def productPrefixMethod: Tree = {
      val method = syntheticMethod(nme.productPrefix, 0, sym => PolyType(List(), StringClass.tpe))
      typer.typed(DefDef(method, Literal(Constant(clazz.name.decode))))
    }

    def productArityMethod(nargs:Int ): Tree = {
      val method = syntheticMethod(nme.productArity, 0, sym => PolyType(List(), IntClass.tpe))
      typer.typed(DefDef(method, Literal(Constant(nargs))))
    }

    def productElementMethod(accs: List[Symbol]): Tree = {
      //val retTpe = lub(accs map (_.tpe.resultType))
      val method = syntheticMethod(nme.productElement, 0,
        sym => MethodType(sym.newSyntheticValueParams(List(IntClass.tpe)), AnyClass.tpe/*retTpe*/))
      typer.typed(DefDef(method, Match(Ident(method.paramss.head.head), {
	(for ((sym,i) <- accs.zipWithIndex) yield {
	  CaseDef(Literal(Constant(i)),EmptyTree, Ident(sym))
	}):::List(CaseDef(Ident(nme.WILDCARD), EmptyTree,
		    Throw(New(TypeTree(IndexOutOfBoundsExceptionClass.tpe), List(List(
		      Select(Ident(method.paramss.head.head), nme.toString_)
		    ))))))
      })))
    }

    def moduleToStringMethod: Tree = {
      val method = syntheticMethod(nme.toString_, FINAL, sym => MethodType(List(), StringClass.tpe))
      typer.typed(DefDef(method, Literal(Constant(clazz.name.decode))))
    }

    def forwardingMethod(name: Name): Tree = {
      val target = getMember(ScalaRunTimeModule, "_" + name)
      val paramtypes =
        if (target.tpe.paramTypes.isEmpty) List()
        else target.tpe.paramTypes.tail
      val method = syntheticMethod(
        name, 0, sym => MethodType(sym.newSyntheticValueParams(paramtypes), target.tpe.resultType))
      typer.typed(DefDef(method,
        Apply(gen.mkAttributedRef(target), This(clazz) :: (method.paramss.head map Ident))))
    }

    def equalsSym =
      syntheticMethod(nme.equals_, 0,
                      sym => MethodType(sym.newSyntheticValueParams(List(AnyClass.tpe)), BooleanClass.tpe))

    /** The equality method for case modules:
     *   def equals(that: Any) = this eq that
     */
    def equalsModuleMethod: Tree = {
      val method = equalsSym
      val methodDef =
        DefDef(method,
          Apply(
            Select(This(clazz), Object_eq),
            List(
              TypeApply(
                Select(
                  Ident(method.paramss.head.head),
                  Any_asInstanceOf),
                List(TypeTree(AnyRefClass.tpe))))))
      localTyper.typed(methodDef)
    }

    /** The equality method for case classes.  The argument is an Any,
     *  but because of boxing it will always be an Object, so a check
     *  is neither necessary nor useful before the cast.
     *
     *   def equals(that: Any) =
     *     (this eq that.asInstanceOf[AnyRef]) ||
     *     (that match {
     *       case this.C(this.arg_1, ..., this.arg_n) => true
     *       case _ => false
     *     })
     */
    def equalsClassMethod: Tree = {
      val method = equalsSym
      val methodDef =
        DefDef(
          method, {
            val that = Ident(method.paramss.head.head)
            val constrParamTypes = clazz.primaryConstructor.tpe.paramTypes
            val hasVarArgs = !constrParamTypes.isEmpty && constrParamTypes.last.typeSymbol == RepeatedParamClass
            val (pat, guard) = {
              val guards = new ListBuffer[Tree]
              val params = for ((acc, cpt) <- clazz.caseFieldAccessors zip constrParamTypes) yield {
                val name = context.unit.fresh.newName(clazz.pos, acc.name+"$")
                val isVarArg = cpt.typeSymbol == RepeatedParamClass
                guards += Apply(
                  Select(
                    Ident(name),
                    if (isVarArg) nme.sameElements else nme.EQ),
                  List(Ident(acc)))
                Bind(name,
                     if (isVarArg) Star(Ident(nme.WILDCARD))
                     else Ident(nme.WILDCARD))
              }
              ( Apply(Ident(clazz.name.toTermName), params),
                if (guards.isEmpty) EmptyTree
                else guards reduceLeft { (g1: Tree, g2: Tree) =>
                  Apply(Select(g1, nme.AMPAMP), List(g2))
                }
              )
            }
            val eq_ = Apply(Select(This(clazz), nme.eq), List(that setType AnyRefClass.tpe))
            val match_ = Match(that, List(
                  CaseDef(pat, guard, Literal(Constant(true))),
                  CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(Constant(false)))))

            gen.mkOr(eq_, match_)
          }
        )
      localTyper.typed(methodDef)
    }

    def hasSerializableAnnotation(clazz: Symbol): Boolean =
      clazz.hasAnnotation(definitions.SerializableAttr)

    def readResolveMethod: Tree = {
      // !!! the synthetic method "readResolve" should be private,
      // but then it is renamed !!!
      val method = newSyntheticMethod(nme.readResolve, PROTECTED,
                                      sym => MethodType(List(), ObjectClass.tpe))
      typer.typed(DefDef(method, gen.mkAttributedRef(clazz.sourceModule)))
    }

    def newAccessorMethod(tree: Tree): Tree = tree match {
      case DefDef(_, _, _, _, _, rhs) =>
        var newAcc = tree.symbol.cloneSymbol
        newAcc.name = context.unit.fresh.newName(tree.symbol.pos, tree.symbol.name + "$")
        newAcc.setFlag(SYNTHETIC).resetFlag(ACCESSOR | PARAMACCESSOR | PRIVATE)
        newAcc = newAcc.owner.info.decls enter newAcc
        val result = typer.typed(DefDef(newAcc, rhs.duplicate))
        log("new accessor method " + result)
        result
    }

    val ts = new ListBuffer[Tree]

    def isPublic(sym: Symbol) =
      !sym.hasFlag(PRIVATE | PROTECTED) && sym.privateWithin == NoSymbol

    if (!phase.erasedTypes) {
      try {
        if (clazz hasFlag CASE) {
          val isTop = !(clazz.info.baseClasses.tail exists (_ hasFlag CASE))
          // case classes are implicitly declared serializable
          clazz.addAnnotation(AnnotationInfo(SerializableAttr.tpe, List(), List()))

          if (isTop) {
            for (stat <- templ.body) {
              if (stat.isDef && stat.symbol.isMethod && stat.symbol.hasFlag(CASEACCESSOR) && !isPublic(stat.symbol)) {
                ts += newAccessorMethod(stat)
                stat.symbol.resetFlag(CASEACCESSOR)
              }
            }
          }
          if (clazz.isModuleClass) {
            if (!hasOverridingImplementation(Object_toString)) ts += moduleToStringMethod
            // if there's a synthetic method in a parent case class, override its equality
            // with eq (see #883)
            val otherEquals = clazz.info.nonPrivateMember(Object_equals.name)
            if (otherEquals.owner != clazz && (otherEquals hasFlag SYNTHETICMETH)) ts += equalsModuleMethod
          } else {
            if (!hasOverridingImplementation(Object_hashCode)) ts += forwardingMethod(nme.hashCode_)
            if (!hasOverridingImplementation(Object_toString)) ts += forwardingMethod(nme.toString_)
            if (!hasOverridingImplementation(Object_equals)) ts += equalsClassMethod
          }

          if (!hasOverridingImplementation(Product_productPrefix)) ts += productPrefixMethod
          val accessors = clazz.caseFieldAccessors
          if (!hasOverridingImplementation(Product_productArity))
            ts += productArityMethod(accessors.length)
          if (!hasOverridingImplementation(Product_productElement))
            ts += productElementMethod(accessors)
        }

        if (clazz.isModuleClass && hasSerializableAnnotation(clazz)) {
          // If you serialize a singleton and then deserialize it twice,
          // you will have two instances of your singleton, unless you implement
          // the readResolve() method (see http://www.javaworld.com/javaworld/
          // jw-04-2003/jw-0425-designpatterns_p.html)
          // question: should we do this for all serializable singletons, or (as currently done)
          // only for those that carry a @serializable annotation?
          if (!hasImplementation(nme.readResolve)) ts += readResolveMethod
        }
      } catch {
        case ex: TypeError =>
          if (!reporter.hasErrors) throw ex
      }
    }
    val synthetics = ts.toList
    treeCopy.Template(
      templ, templ.parents, templ.self, if (synthetics.isEmpty) templ.body else templ.body ::: synthetics)
  }
}
