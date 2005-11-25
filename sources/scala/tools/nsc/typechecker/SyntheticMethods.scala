/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.typechecker;

import symtab.Flags._;
import util.ListBuffer;

/**
 *   - caseArity, caseElement implementations added to case classes
 *   - equals, and hashCode and toString methods are added to case classes,
 *       unless they are defined in the class or a baseclass
 *       different from java.lang.Object
 *   - toString method is added to case objects,
 *       unless they are defined in the class or a baseclass
 *       different from java.lang.Object
*/
[_trait_] abstract class SyntheticMethods: Analyzer {
  import global._;                  // the global environment
  import definitions._;             // standard classes and methods
  import typer.{typed};             // methods to type trees

  def addSyntheticMethods(templ: Template, clazz: Symbol): Template = {

    def hasImplementation(name: Name): boolean = {
      val sym = clazz.info.nonPrivateMember(name);
      sym.isTerm &&
      (sym.owner == clazz ||
       !(ObjectClass isSubClass sym.owner) && !(sym hasFlag DEFERRED));
    }

    def syntheticMethod(name: Name, flags: int, tpe: Type) = {
      val method = clazz.newMethod(clazz.pos, name) setFlag (flags | OVERRIDE) setInfo tpe;
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

    def readResolveMethod: Tree = {
      // !!! the synthetic method "readResolve" should be private,
      // but then it is renamed !!!
      val method = syntheticMethod(nme.readResolve, PROTECTED, MethodType(List(), ObjectClass.tpe));
      typed(DefDef(method, vparamss => gen.mkRef(clazz.sourceModule)))
    }

    val ts = new ListBuffer[Tree];
    if ((clazz hasFlag CASE) && !phase.erasedTypes) {
      ts += tagMethod;
      if (clazz.isModuleClass) {
	if (!hasImplementation(nme.toString_)) ts += moduleToStringMethod;
	if (clazz.isSubClass(SerializableClass)) {
	  // If you serialize a singleton and then deserialize it twice,
	  // you will have two instances of your singleton, unless you implement
	  // the readResolve() method (see http://www.javaworld.com/javaworld/
	  // jw-04-2003/jw-0425-designpatterns_p.html)
	  if (!hasImplementation(nme.readResolve)) ts += readResolveMethod;
	}
      } else {
	if (!hasImplementation(nme.caseElement)) ts += caseElementMethod;
	if (!hasImplementation(nme.caseArity)) ts += caseArityMethod;
	if (!hasImplementation(nme.caseName)) ts += caseNameMethod;
	if (!hasImplementation(nme.equals_)) ts += forwardingMethod(nme.equals_);
	if (!hasImplementation(nme.hashCode_)) ts += forwardingMethod(nme.hashCode_);
	if (!hasImplementation(nme.toString_)) ts += forwardingMethod(nme.toString_);
      }
    }
    val synthetics = ts.toList;
    copy.Template(
      templ, templ.parents, if (synthetics.isEmpty) templ.body else templ.body ::: synthetics)
  }
}
