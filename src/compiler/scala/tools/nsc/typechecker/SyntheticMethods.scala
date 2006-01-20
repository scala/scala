/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.typechecker;

import symtab.Flags._;
import util.FreshNameCreator;
import scala.collection.mutable.ListBuffer;

/**
 *   - caseArity, caseElement implementations added to case classes
 *   - equals, and hashCode and toString methods are added to case classes,
 *       unless they are defined in the class or a baseclass
 *       different from java.lang.Object
 *   - toString method is added to case objects,
 *       unless they are defined in the class or a baseclass
 *       different from java.lang.Object
*/
mixin class SyntheticMethods requires Analyzer {
  import global._;                  // the global environment
  import definitions._;             // standard classes and methods
  import typer.{typed};             // methods to type trees

  def addSyntheticMethods(templ: Template, clazz: Symbol, unit: CompilationUnit): Template = {

    def hasImplementation(name: Name): boolean = {
      val sym = clazz.info.nonPrivateMember(name);
      (sym.isTerm &&
       (sym.owner == clazz ||
        !(ObjectClass isSubClass sym.owner) && !(sym hasFlag DEFERRED)))
    }

    def syntheticMethod(name: Name, flags: int, tpe: Type) =
      newSyntheticMethod(name, flags | OVERRIDE, tpe);

    def newSyntheticMethod(name: Name, flags: int, tpe: Type) = {
      val method = clazz.newMethod(clazz.pos, name) setFlag (flags) setInfo tpe;
      clazz.info.decls.enter(method);
      method
    }

    def caseElementMethod: Tree = {
      val method = syntheticMethod(
	nme.caseElement, FINAL, MethodType(List(IntClass.tpe), AnyClass.tpe));
      val caseFields = clazz.caseFieldAccessors map gen.mkRef;
      typed(
	DefDef(method, vparamss =>
	  if (caseFields.isEmpty) Literal(Constant(null))
	  else {
	    var i = caseFields.length;
	    var cases = List(CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(Constant(null))));
	    for (val field <- caseFields.reverse) {
	      i = i - 1; cases = CaseDef(Literal(Constant(i)), EmptyTree, field) :: cases
	    }
	    Match(Ident(vparamss.head.head), cases)
	  }))
    }

    def caseArityMethod: Tree = {
      val method = syntheticMethod(nme.caseArity, FINAL, PolyType(List(), IntClass.tpe));
      typed(DefDef(method, vparamss => Literal(Constant(clazz.caseFieldAccessors.length))))
    }

    def caseNameMethod: Tree = {
      val method = syntheticMethod(nme.caseName, FINAL, PolyType(List(), StringClass.tpe));
      typed(DefDef(method, vparamss => Literal(Constant(clazz.name.decode))))
    }

    def moduleToStringMethod: Tree = {
      val method = syntheticMethod(nme.toString_, FINAL, MethodType(List(), StringClass.tpe));
      typed(DefDef(method, vparamss => Literal(Constant(clazz.name.decode))))
    }

    def tagMethod: Tree = {
      val method = syntheticMethod(nme.tag, FINAL, MethodType(List(), IntClass.tpe));
      typed(DefDef(method, vparamss => Literal(Constant(clazz.tag))))
    }

    def forwardingMethod(name: Name): Tree = {
      val target = getMember(ScalaRunTimeModule, "_" + name);
      val method = syntheticMethod(
	name, 0, MethodType(target.tpe.paramTypes.tail, target.tpe.resultType));
      typed(DefDef(method, vparamss =>
	Apply(gen.mkRef(target), This(clazz) :: (vparamss.head map Ident))));
    }

    val SerializableAttr = definitions.SerializableAttr;

    def isSerializable(clazz: Symbol): Boolean =
      clazz.attributes.exists(p => p match {
        case Pair(SerializableAttr, _) => true;
        case _ => false
      })

    def readResolveMethod: Tree = {
      // !!! the synthetic method "readResolve" should be private,
      // but then it is renamed !!!
      val method = newSyntheticMethod(nme.readResolve, PROTECTED,
                                      MethodType(List(), ObjectClass.tpe));
      typed(DefDef(method, vparamss => gen.mkRef(clazz.sourceModule)))
    }

    def newAccessorMethod(tree: Tree): Tree = tree match {
      case DefDef(_, _, _, _, _, rhs) =>
        val newAcc = tree.symbol.cloneSymbol;
        newAcc.name = unit.fresh.newName(""+tree.symbol.name+"$");
        newAcc.setFlag(SYNTHETIC).resetFlag(ACCESSOR | PARAMACCESSOR);
        newAcc.owner.info.decls enter newAcc;
        val result = typed(DefDef(newAcc, vparamss => rhs.duplicate));
        System.out.println("new acc method " + result)
        result
    }

    val ts = new ListBuffer[Tree];
    if ((clazz hasFlag CASE) && !phase.erasedTypes) {
      // case classes are implicitly declared serializable
      clazz.attributes = Pair(SerializableAttr, List()) :: clazz.attributes;

      for (val stat <- templ.body) {
        if (stat.isDef && stat.symbol.isMethod && stat.symbol.hasFlag(CASEACCESSOR) &&
            (stat.symbol.hasFlag(PRIVATE | PROTECTED) || stat.symbol.privateWithin != NoSymbol)) {
          ts += newAccessorMethod(stat);
          stat.symbol.resetFlag(CASEACCESSOR)
        }
      }

      ts += tagMethod;
      if (clazz.isModuleClass) {
	if (!hasImplementation(nme.toString_)) ts += moduleToStringMethod;
      } else {
	if (!hasImplementation(nme.hashCode_)) ts += forwardingMethod(nme.hashCode_);
	if (!hasImplementation(nme.toString_)) ts += forwardingMethod(nme.toString_);
        if (!hasImplementation(nme.equals_)) ts += forwardingMethod(nme.equals_);
      }
      if (!hasImplementation(nme.caseElement)) ts += caseElementMethod;
      if (!hasImplementation(nme.caseArity)) ts += caseArityMethod;
      if (!hasImplementation(nme.caseName)) ts += caseNameMethod;
    }
    if (!phase.erasedTypes && clazz.isModuleClass && isSerializable(clazz)) {
      // If you serialize a singleton and then deserialize it twice,
      // you will have two instances of your singleton, unless you implement
      // the readResolve() method (see http://www.javaworld.com/javaworld/
      // jw-04-2003/jw-0425-designpatterns_p.html)
      if (!hasImplementation(nme.readResolve)) ts += readResolveMethod;
    }
    val synthetics = ts.toList;
    copy.Template(
      templ, templ.parents, if (synthetics.isEmpty) templ.body else templ.body ::: synthetics)
  }
}
