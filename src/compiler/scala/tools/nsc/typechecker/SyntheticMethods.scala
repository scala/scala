/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.typechecker

import symtab.Flags._
import util.FreshNameCreator
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
trait SyntheticMethods requires Analyzer {
  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed}             // methods to type trees

  /**
   *  @param templ ...
   *  @param clazz ...
   *  @param unit  ...
   *  @return      ...
   */
  def addSyntheticMethods(templ: Template, clazz: Symbol, unit: CompilationUnit): Template = {

    def hasImplementation(name: Name): Boolean = {
      val sym = clazz.info.nonPrivateMember(name)
      (sym.isTerm &&
       (sym.owner == clazz ||
        !(ObjectClass isNonBottomSubClass sym.owner) && !(sym hasFlag DEFERRED)))
    }

    def hasDirectImplementation(name: Name): Boolean = {
      val sym = clazz.info.nonPrivateMember(name)
      (sym.isTerm && sym.owner == clazz)
    }

    def syntheticMethod(name: Name, flags: Int, tpe: Type) =
      newSyntheticMethod(name, flags | OVERRIDE, tpe)

    def newSyntheticMethod(name: Name, flags: Int, tpe: Type) = {
      val method = clazz.newMethod(clazz.pos, name) setFlag (flags) setInfo tpe
      clazz.info.decls.enter(method)
      method
    }

    /*
    def productSelectorMethod(n: int, accessor: Symbol): Tree = {
      val method = syntheticMethod(newTermName("_"+n), FINAL, accessor.tpe)
      typed(DefDef(method, vparamss => gen.mkAttributedRef(accessor)))
    }
    */
    def productPrefixMethod: Tree = {
      val method = syntheticMethod(nme.productPrefix, FINAL, PolyType(List(), StringClass.tpe))
      typed(DefDef(method, vparamss => Literal(Constant(clazz.name.decode))))
    }

    def productArityMethod(nargs:Int ): Tree = {
      val method = syntheticMethod(nme.arity, FINAL, PolyType(List(), IntClass.tpe))
      typed(DefDef(method, vparamss => Literal(Constant(nargs))))
    }

    def productElementMethod(accs: List[Symbol]): Tree = {
      //val retTpe = lub(accs map (.tpe.resultType))
      val method = syntheticMethod(nme.element, FINAL, MethodType(List(IntClass.tpe), AnyClass.tpe/*retTpe*/))
      typed(DefDef(method, vparamss => Match(Ident(vparamss.head.head), {
	(for(val (sym,i) <- accs.zipWithIndex) yield {
	  CaseDef(Literal(Constant(i)),EmptyTree, Ident(sym))
	}):::List(CaseDef(Ident(nme.WILDCARD), EmptyTree,
		    Throw(New(TypeTree(IndexOutOfBoundsExceptionClass.tpe), List(List(
		      Select(Ident(vparamss.head.head), nme.toString_)
		    ))))))
      })))
    }

    def moduleToStringMethod: Tree = {
      val method = syntheticMethod(nme.toString_, FINAL, MethodType(List(), StringClass.tpe))
      typed(DefDef(method, vparamss => Literal(Constant(clazz.name.decode))))
    }

    def tagMethod: Tree = {
      val method = syntheticMethod(nme.tag, FINAL, MethodType(List(), IntClass.tpe))
      typed(DefDef(method, vparamss => Literal(Constant(clazz.tag))))
    }

    def forwardingMethod(name: Name): Tree = {
      val target = getMember(ScalaRunTimeModule, "_" + name)
      val paramtypes =
        if (target.tpe.paramTypes.isEmpty) List()
        else target.tpe.paramTypes.tail
      val method = syntheticMethod(
        name, 0, MethodType(paramtypes, target.tpe.resultType))
      typed(DefDef(method, vparamss =>
        Apply(gen.mkAttributedRef(target), This(clazz) :: (vparamss.head map Ident))))
    }

    def equalsMethod: Tree = {
      val constrParamTypes = clazz.primaryConstructor.tpe.paramTypes
      val hasVarArgs = !constrParamTypes.isEmpty && constrParamTypes.last.symbol == RepeatedParamClass
      val target = getMember(ScalaRunTimeModule, if (hasVarArgs) nme._equalsWithVarArgs else nme._equals)
      val paramtypes =
        if (target.tpe.paramTypes.isEmpty) List()
        else target.tpe.paramTypes.tail
      val method = syntheticMethod(
       nme.equals_, 0, MethodType(paramtypes, target.tpe.resultType))
      typed(DefDef(method, vparamss =>
        Apply(
          Select(
            Apply(
              Select(Ident(vparamss.head.head), Object_eq),
              List(This(clazz))),
            Boolean_or),
          List(
            Apply(
              Select(
                TypeApply(
                  Select(Ident(vparamss.head.head), Any_isInstanceOf),
                  List(TypeTree(clazz.tpe))),
                Boolean_and),
              List(
                Apply(gen.mkAttributedRef(target),
                      This(clazz) :: (vparamss.head map Ident))))))));
    }

    def isSerializable(clazz: Symbol): Boolean =
      !clazz.getAttributes(definitions.SerializableAttr).isEmpty

    def readResolveMethod: Tree = {
      // !!! the synthetic method "readResolve" should be private,
      // but then it is renamed !!!
      val method = newSyntheticMethod(nme.readResolve, PROTECTED,
                                      MethodType(List(), ObjectClass.tpe))
      typed(DefDef(method, vparamss => gen.mkAttributedRef(clazz.sourceModule)))
    }

    def newAccessorMethod(tree: Tree): Tree = tree match {
      case DefDef(_, _, _, _, _, rhs) =>
        val newAcc = tree.symbol.cloneSymbol
        newAcc.name = unit.fresh.newName("" + tree.symbol.name + "$")
        newAcc.setFlag(SYNTHETIC).resetFlag(ACCESSOR | PARAMACCESSOR)
        newAcc.owner.info.decls enter newAcc
        val result = typed(DefDef(newAcc, vparamss => rhs.duplicate))
        log("new accessor method " + result)
        result
    }

    def beanSetterOrGetter(sym: Symbol): Symbol =
      if (!sym.name(0).isLetter) {
        unit.error(sym.pos, "attribute `BeanProperty' can be applied only to fields that start with a letter")
        NoSymbol
      } else {
        var name0 = sym.name
        if (sym.isSetter) name0 = nme.setterToGetter(name0)
        val prefix = if (sym.isSetter) "set" else
          if (sym.tpe.resultType == BooleanClass.tpe) "is" else "get"
        val arity = if (sym.isSetter) 1 else 0
        val name1 = prefix + name0(0).toUpperCase + name0.subName(1, name0.length)
        val sym1 = clazz.info.decl(name1)
        if (sym1 != NoSymbol && sym1.tpe.paramTypes.length == arity) {
          unit.error(sym.pos, "a definition of `"+name1+"' already exists in " + clazz)
          NoSymbol
        } else {
          clazz.newMethod(sym.pos, name1)
            .setInfo(sym.info)
            .setFlag(sym.getFlag(DEFERRED | OVERRIDE | STATIC))
        }
      }

    val ts = new ListBuffer[Tree]

    def addBeanGetterMethod(sym: Symbol) = {
      val getter = beanSetterOrGetter(sym)
      if (getter != NoSymbol)
        ts += typed(DefDef(
          getter,
          vparamss => if (sym hasFlag DEFERRED) EmptyTree else gen.mkAttributedRef(sym)))
    }

    def addBeanSetterMethod(sym: Symbol) = {
      val setter = beanSetterOrGetter(sym)
      if (setter != NoSymbol)
        ts += typed(DefDef(
          setter,
          vparamss =>
            if (sym hasFlag DEFERRED) EmptyTree
            else Apply(gen.mkAttributedRef(sym), List(Ident(vparamss.head.head)))))
    }

    def isPublic(sym: Symbol) =
      !sym.hasFlag(PRIVATE | PROTECTED) && sym.privateWithin == NoSymbol

    if (!phase.erasedTypes) {

      if (clazz hasFlag CASE) {
        // case classes are implicitly declared serializable
        clazz.attributes = AnnotationInfo(SerializableAttr.tpe, List(), List()) :: clazz.attributes

        for (val stat <- templ.body) {
          if (stat.isDef && stat.symbol.isMethod && stat.symbol.hasFlag(CASEACCESSOR) && !isPublic(stat.symbol)) {
            ts += newAccessorMethod(stat)
            stat.symbol.resetFlag(CASEACCESSOR)
          }
        }

        if (clazz.info.nonPrivateDecl(nme.tag) == NoSymbol) ts += tagMethod
        if (clazz.isModuleClass) {
          if (!hasImplementation(nme.toString_)) ts += moduleToStringMethod
        } else {
          if (!hasImplementation(nme.hashCode_)) ts += forwardingMethod(nme.hashCode_)
          if (!hasImplementation(nme.toString_)) ts += forwardingMethod(nme.toString_)
          if (!hasImplementation(nme.equals_)) ts += equalsMethod
        }

        if (!hasDirectImplementation(nme.productPrefix)) ts += productPrefixMethod
	val accessors = clazz.caseFieldAccessors
	if (!hasImplementation(nme.arity))
	  ts += productArityMethod(accessors.length)
	if (!hasImplementation(nme.element))
	  ts += productElementMethod(accessors)
      }

      if (clazz.isModuleClass && isSerializable(clazz)) {
        // If you serialize a singleton and then deserialize it twice,
        // you will have two instances of your singleton, unless you implement
        // the readResolve() method (see http://www.javaworld.com/javaworld/
        // jw-04-2003/jw-0425-designpatterns_p.html)
        if (!hasImplementation(nme.readResolve)) ts += readResolveMethod
      }
      if (!forCLDC && !forMSIL)
        for (val sym <- clazz.info.decls.toList)
          if (!sym.getAttributes(BeanPropertyAttr).isEmpty)
            if (sym.isGetter)
              addBeanGetterMethod(sym)
            else if (sym.isSetter)
              addBeanSetterMethod(sym)
            else if (sym.isMethod || sym.isType)
              unit.error(sym.pos, "attribute `BeanProperty' is not applicable to " + sym)
    }
    val synthetics = ts.toList
    copy.Template(
      templ, templ.parents, if (synthetics.isEmpty) templ.body else templ.body ::: synthetics)
  }
}
