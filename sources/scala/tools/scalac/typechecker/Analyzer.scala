/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

// todo: eliminate Typed nodes.
// todo: use SELECTOR flag to avoid access methods for privates
// todo: use mangled name or drop.
// todo: emit warnings for unchecked.
// todo: synchronize on module instantiation.
// todo: empty package

import ch.epfl.lamp.util.Position;
import scalac._;
import scalac.util._;
import scalac.ast._;
import scalac.ast.printer._;
import scalac.symtab._;
import scalac.symtab.classfile._;
import Tree._;
import java.util.HashMap;
import java.lang.{Boolean, Byte, Short, Character, Integer, Object}
import scala.tools.scalac.util.NewArray;
import scalac.{Global => scalac_Global}

package scala.tools.scalac.typechecker {

/** The main attribution phase.
 */
class Analyzer(global: scalac_Global, descr: AnalyzerPhase) extends Transformer(global) {

  import Modifiers._;
  import Kinds._;

  val definitions = global.definitions;
  val infer = new scala.tools.scalac.typechecker.Infer(this);
  val desugarize = new DeSugarize(make, copy, gen, infer, global);
  val constfold = new ConstantFolder(this);

  var unit: Unit = _;

  private var context: Context = _;
  private var pt: Type = _;
  private var mode: int = _;

  private var inAlternative: boolean = _;
  private var patternVars: HashMap = _;   // for pattern matching; maps x to {true,false}

  override def apply(units: Array[Unit]): unit = {
    var i = 0; while (i <  units.length) {
      enterUnit(units(i));
      i = i + 1
    }
    super.apply(units);
    var n = descr.newSources.size();
    while (n > 0) { // this calls apply(u) for every unit `u'.
      val l = global.units.length;
      val newUnits = new Array[Unit](l + n);
      System.arraycopy(global.units, 0, newUnits, 0, l);
      var i = 0; while (i < n) {
        newUnits(i + l) = descr.newSources.get(i).asInstanceOf[Unit];
	i = i + 1
      }
      global.units = newUnits;
      descr.newSources.clear();
      var j = l; while (j < newUnits.length) {
        apply(newUnits(j));
	j = j + 1
      }
      n = descr.newSources.size();
    }
  }

  def enterUnit(unit: Unit): unit =
    enter(
      new Context(
	Tree.Empty,
	if (unit.console) descr.consoleContext else descr.startContext),
      unit);

  def enter(context: Context, unit: Unit): unit = {
    assert(this.unit == null, "start unit non null for " + unit);
    this.unit = unit;
    this.context = context;
    this.patternVars = new HashMap();
    val prevImports = context.imports;
    descr.contexts.put(unit, context);
    enterSyms(unit.body);
    context.imports = prevImports;
    this.unit = null;
    this.context = null;
  }

  def lateEnter(unit: Unit, sym: Symbol): unit = {
    enterUnit(unit);
    if (sym.rawFirstInfo().isInstanceOf[SourceCompleter]) {
      sym.setInfo(Type.ErrorType);
      val kind = if (sym.name.isTermName()) "object " else "class ";
      throw new Type$Error("file " + unit.source + " does not define public " +
			   kind + sym.fullName());
    } else {
      descr.newSources.add(unit);
    }
  }

  override def apply(unit: Unit): unit = {
    global.log("checking " + unit);
    assert(this.unit == null, "start unit non null for " + unit);
    this.unit = unit;
    this.context = descr.contexts.remove(unit).asInstanceOf[Context];
    assert(this.context != null, "could not find context for " + unit);
    unit.body = transformStatSeq(unit.body, Symbol.NONE);
    if (global.target != scalac_Global.TARGET_INT && global.reporter.errors() == 0) {
      genSymData(unit.body);
    }
    this.unit = null;
    this.context = null;
    global.operation("checked " + unit);
  }

  def genSymData(stats: Array[Tree]): unit = {
    var i = 0; while (i < stats.length) {
      stats(i) match {
	case Tree$ClassDef(_, _, _, _, _, _) | Tree$ModuleDef(_, _, _, _) =>
	  val sym = stats(i).symbol();
	  val fullname = sym.fullName();
	  if (global.symdata.get(fullname) == null) {
	    val pickle: Pickle = new Pickle();
	    pickle.add(sym.owner().info().lookup(sym.name.toTermName()));
	    pickle.add(sym.owner().info().lookup(sym.name.toTypeName()));
	    pickle.pickle();
	    global.symdata.put(fullname, pickle);
	  }
	case Tree$PackageDef(packaged, templ) =>
	  genSymData(templ.body);
	case _ =>
      }
      i = i + 1
    }
  }

  /** Mode constants
  */
  val NOmode        = 0x000;
  val EXPRmode      = 0x001;  // these 4 modes are mutually exclusive.
  val PATTERNmode   = 0x002;
  val CONSTRmode    = 0x004;
  val TYPEmode      = 0x008;

  val FUNmode       = 0x10;   // orthogonal to above. When set
                              // we are looking for a method or constructor

  val POLYmode      = 0x020;  // orthogonal to above. When set
                              // expression types can be polymorphic.

  val QUALmode      = 0x040;  // orthogonal to above. When set
			      // expressions may be packages and
                              // Java statics modules.

  val SUPERmode     = 0x080;  // Goes with CONSTRmode. When set
                              // we are checking a superclass
                              // constructor invocation.

  val baseModes     = EXPRmode | PATTERNmode | CONSTRmode;

  val SEQUENCEmode  = 0x1000;  // orthogonal to above. When set
                               // we turn "x" into "x@_"
                               // and allow args to be of type Seq( a) instead of a

// Diagnostics ----------------------------------------------------------------

  def errorTree(pos: int): Tree =
    make.Bad(pos).setSymbol(Symbol.ERROR).setType(Type.ErrorType);

  def error(pos: int, msg: String): Tree = {
    unit.error(pos, msg);
    errorTree(pos);
  }

  def typeError(pos: int, found: Type, req: Type): unit = {
    var msg: String = infer.typeErrorMsg("type mismatch", found, req);
    val foundResult: Type = found.resultType();
    if (foundResult != found && infer.isCompatible(foundResult, req))
      msg = msg + "\n possible cause: missing arguments for method or constructor";
    error(pos, msg);
  }

  def reportTypeError(pos: int, ex: Type$Error): Tree = {
    if (global.debug) ex.printStackTrace();
    if (ex.isInstanceOf[CyclicReference]) {
      val cyc: CyclicReference = ex.asInstanceOf[CyclicReference];
      if (cyc.info.isInstanceOf[LazyTreeType]) {
	(cyc.info.asInstanceOf[LazyTreeType]).tree match {
	  case Tree$ValDef(_, _, Tree.Empty, _) =>
	    return error(pos, "recursive " + cyc.sym + " needs type");
	  case Tree$DefDef(_, _, _, _, Tree.Empty, _) =>
	    return error(pos, "recursive function " + cyc.sym.name + " needs result type");
	  case _ =>
	}
      }
    }
    error(pos, ex.msg);
  }

// Name resolution -----------------------------------------------------------

  def decode(name: Name): String =
    if (name.isTypeName()) "type " + NameTransformer.decode(name);
    else "value " + NameTransformer.decode(name);

  /** Check that `sym' is accessible as a member of tree `site' in current context.
  */
  def checkAccessible(pos: int, sym: Symbol, symtype: Type, site: Tree, sitetype: Type): Type = {
    //System.out.println("check acc " + sym);//DEBUG
    if ((sym.owner().flags & INCONSTRUCTOR) != 0 &&
	!(sym.kind == TYPE && sym.isParameter()) &&
	site.isInstanceOf[Tree$This]) {
      error(pos, "" + sym + " cannot be accessed from constructor");
      Type.ErrorType;
    } else {
      symtype match {
	case Type$OverloadedType(alts, alttypes) =>
	  var nacc: int = 0;
	  var i = 0; while (i < alts.length) {
	    if (isAccessible(alts(i), site, sitetype))
	      nacc = nacc + 1;
	    i = i + 1
	  }
	  if (nacc == 0) {
	    error(pos, "" + sym + " cannot be accessed in " + sitetype.widen());
	    Type.ErrorType
	  } else {
	    val alts1: Array[Symbol] = new Array[Symbol](nacc);
	    val alttypes1: Array[Type] = new Array[Type](nacc);
	    nacc = 0;
	    var i = 0; while (i < alts.length) {
	      if (isAccessible(alts(i), site, sitetype)) {
		alts1(nacc) = alts(i);
		alttypes1(nacc) = alttypes(i);
		nacc = nacc + 1;
	      }
	      i = i + 1
	    }
	    new Type$OverloadedType(alts1, alttypes1)
	  }
	case _ =>
	  if (isAccessible(sym, site, sitetype)) {
	    symtype
	  } else {
	    error(pos, "" + sym + " cannot be accessed in " + sitetype.widen());
	    Type.ErrorType
	  }
      }
    }
  }

  /** Is `sym' accessible as a member of tree `site' in current context?
  */
  private def isAccessible(sym: Symbol, site: Tree, sitetype: Type): boolean = {

    /** Are we inside definition of `owner'?
    */
    def accessWithin(owner: Symbol): boolean = {
      var c: Context = context;
      while (c != Context.NONE && c.owner != owner) {
	c = c.outer.enclClass;
      }
      c != Context.NONE;
    }

    /** Is `clazz' a subclass of an enclosing class?
    */
    def isSubClassOfEnclosing(clazz: Symbol): boolean = {
      var c: Context = context;
      while (c != Context.NONE && !clazz.isSubClass(c.owner)) {
	c = c.outer.enclClass;
      }
      c != Context.NONE;
    }

    (sym.flags & (PRIVATE | PROTECTED)) == 0
    ||
    {val owner = if (sym.isConstructor()) sym.constructorClass()
		  else sym.owner();
     accessWithin(owner)
     ||
     ((sym.flags & PRIVATE) == 0) &&
     sitetype.symbol().isSubClass(owner) &&
     (site.isInstanceOf[Tree$Super] || isSubClassOfEnclosing(sitetype.symbol()))
    }
  }

// Checking methods ----------------------------------------------------------

  /** Check that symbol's definition is well-formed. This means:
  *   - no conflicting modifiers
  *   - `abstract' modifier only for classes
  *   - `override' modifier never for classes
  *   - `def' modifier never for parameters of case classes
  *   - declarations only in traits or abstract classes
  *   - symbols with `override' modifier override some other symbol.
  */
  def validate(sym: Symbol): unit = {
    if ((sym.flags & (ABSTRACT | OVERRIDE)) == ABSTRACT && sym.kind != CLASS) {
      error(sym.pos, "`abstract' modifier can be used only for classes; " +
	      "\nit should be omitted for abstract members");
    }
    if ((sym.flags & OVERRIDE) != 0 && sym.kind == CLASS) {
      error(sym.pos, "`override' modifier not allowed for classes");
    }
    if ((sym.flags & DEF) != 0 && sym.owner().isPrimaryConstructor() &&
	(sym.owner().constructorClass().flags & CASE) != 0) {
      error(sym.pos, "`def' modifier not allowed for case class parameters");
    }
    /*!!!
    if ((sym.flags & REPEATED) != 0 && sym.owner().isPrimaryConstructor()) {
      error(sym.pos, "`*' modifier not allowed for class parameters");
    }
    */
    if ((sym.flags & DEFERRED) != 0) {
      if (sym.owner().kind != CLASS || (sym.owner().flags & MODUL) != 0 || sym.owner().isAnonymousClass()) {
	error(sym.pos,
	      "only classes can have declared but undefined members" +
	      (if ((sym.flags & MUTABLE) == 0) ""
	       else "\n(Note that variables need to be initialized to be defined)"));
	sym.flags = sym.flags & ~DEFERRED;
      }
    }
    checkNoConflict(sym, DEFERRED, PRIVATE);
    checkNoConflict(sym, FINAL, SEALED);
    checkNoConflict(sym, FINAL, PRIVATE);
    checkNoConflict(sym, PRIVATE, PROTECTED);
    checkNoConflict(sym, PRIVATE, OVERRIDE);
    checkNoConflict(sym, DEFERRED, FINAL);
  }

  /** Check that
  *  - all parents are class types
  *  - supertype conforms to supertypes of all mixin types.
  *  - final classes are only inherited by classes which are
  *    nested within definition of base class, or that occur within same
  *    statement sequence.
  *  - self-type of current class is a subtype of self-type of each parent class.
  *  - parent constructors do not refer to value parameters of class.
  *  - no two parents define same symbol.
  */
  def validateParentClasses(constrs: Array[Tree], parents: Array[Type], selfType: Type): unit = {
    var i = 0;
    while (i < parents.length) {
      if (!checkClassType(constrs(i).pos, parents(i))) return;
      val bsym: Symbol = parents(i).symbol();
      if (i == 0) {
	if ((bsym.flags & (JAVA | INTERFACE)) == (JAVA | INTERFACE))
	  error(constrs(0).pos, "superclass may not be a Java interface");
      } else {
	if ((bsym.flags & (JAVA | INTERFACE)) == JAVA)
	  error(constrs(i).pos, "Java class may not be used as mixin");
	val grandparents = parents(i).parents();
	if (grandparents.length > 0 && !parents(0).isSubType(grandparents(0)))
	  error(constrs(i).pos, "illegal inheritance;\n " + parents(0) +
		" does not conform to " + parents(i) + "'s supertype");
      }
      if ((bsym.flags & FINAL) != 0) {
	error(constrs(i).pos, "illegal inheritance from final class");
      } else if (bsym.isSealed() ||
		 bsym.isSubClass(definitions.ANYVAL_CLASS) ||
		 bsym.isSubClass(definitions.ARRAY_CLASS)) {
	// are we in same scope as base type definition?
	val e: Scope$Entry = context.scope.lookupEntry(bsym.name);
	if (e.sym != bsym) {
	  // we are not within same statement sequence
	  var c: Context = context;
	  while (c != Context.NONE && c.owner !=  bsym)
	    c = c.outer;
	  if (c == Context.NONE) {
	    error(constrs(i).pos, "illegal inheritance from sealed class");
	  }
	}
      }
      if (!selfType.isSubType(parents(i).instanceType())) {
	error(constrs(i).pos, "illegal inheritance;\n self-type " +
	      selfType + " does not conform to " + parents(i) +
	      "'s selftype " + parents(i).instanceType());
      }
      var j = 0; while (j < i) {
	if (parents(i).symbol() == parents(j).symbol())
	  error(constrs(i).pos, "" + parents(i).symbol() + " is inherited twice");
	j = j + 1
      }
      i = i + 1;
    }
  }

  /** Check that type is a class type.
  */
  private def checkClassType(pos: int, tp: Type): boolean = {
    tp.unalias() match {
      case Type$TypeRef(_, sym, _) =>
	if (sym.kind == CLASS) return true;
	else if (sym.kind == ERROR) return false;
      case Type.ErrorType =>
	return false;
      case _ =>
    }
    error(pos, "class type expected");
    false
  }

  /** Check that type is an object type
  */
  private def checkObjectType(pos: int, tp: Type): Type =
    if (tp.isObjectType()) tp
    else {
      if (tp != Type.ErrorType) error(pos, "object type expected");
      Type.ErrorType
    }

  /** Check that type is eta-expandable (i.e. no `def' or `*' parameters)
  */
  def checkEtaExpandable(pos: int, tp: Type): unit = tp match {
    case Type$MethodType(params, restype) =>
      var i = 0; while (i < params.length) {
	if ((params(i).flags & DEF) != 0)
	  error(pos, "method with `def' parameters needs to be fully applied");
	if ((params(i).flags & REPEATED) != 0)
	  error(pos, "method with `*' parameters needs to be fully applied");
	i = i + 1
      }
      checkEtaExpandable(pos, restype);
    case _ =>
  }

  /** Check that `sym' does not contain both `flag1' and `flag2'
  */
  def checkNoConflict(sym: Symbol, flag1: int, flag2: int): unit = {
    if ((sym.flags & (flag1 | flag2)) == (flag1 | flag2)) {
      if (flag1 == DEFERRED)
	error(sym.pos, "abstract member may not have " +
	      Modifiers$Helper.toString(flag2) + " modifier");
      else
	error(sym.pos, "illegal combination of modifiers: " +
	      Modifiers$Helper.toString(flag1) + " and " +
	      Modifiers$Helper.toString(flag2));
    }
  }

  /** Check that type `tp' is not a subtype of itself.
  */
  def checkNonCyclic(pos: int, tp: Type): unit = tp match {
    case Type$TypeRef(pre, sym, args) =>
      sym.initialize();
      if ((sym.flags & LOCKED) != 0) {
	error(pos, "cyclic aliasing or subtyping involving " + sym);
      } else if (sym.kind == ALIAS || sym.kind == TYPE) {
	sym.flags = sym.flags | LOCKED;
	//System.out.println("checking " + sym);//DEBUG
	checkNonCyclic(
	  pos, pre.memberInfo(sym).subst(sym.typeParams(), args));
	if (sym.kind == TYPE)
	  checkNonCyclic(
	    pos, pre.memberLoBound(sym).subst(sym.typeParams(), args));
	sym.flags = sym.flags & ~LOCKED;
      }

    case Type$CompoundType(parents, members) =>
      var i = 0; while (i < parents.length) {
	checkNonCyclic(pos, parents(i));
	i = i + 1
      }

    case Type$SingleType(pre, sym) =>
      sym.initialize();
      if ((sym.flags & LOCKED) != 0) {
	error(pos, "cyclic aliasing or subtyping involving " + sym);
      }

    case _ =>
  }

  /** Check that type does not refer to components defined in current scope.
  */
  def checkNoEscape(pos: int, tp: Type): Type = {

    val checkNoEscapeMap = new Type$Map() {
      override def apply(t: Type): Type = {
	t.unalias() match {
	  case Type$TypeRef(pre, sym, args) =>
	    if (pre.isInstanceOf[Type$ThisType]) checkNoEscape(t, sym);
	  case Type$SingleType(Type$ThisType(_), sym) =>
	    checkNoEscape(t, sym);
	  case _ =>
	}
	return map(t);
      }
      private def checkNoEscape(t: Type, sym: Symbol): unit = {
	val e: Scope$Entry = context.scope.lookupEntry(sym.name);
	if (e.sym == sym && e.owner == context.scope &&
	    !(e.sym.kind == TYPE && (e.sym.flags & PARAM) != 0)) {
	      throw new Type$Error(
		"type " + t + " escapes its defining scope");
	    }
      }
    };

    try {
      checkNoEscapeMap.apply(tp)
    } catch {
      case ex: Type$Error =>
	if ((mode & EXPRmode) != 0 && infer.isFullyDefined(pt)) pt
	else {
	  error(pos, ex.msg + " as part of " + tp.unalias());
	  Type.ErrorType
	}
    }
  }

  /** Check that there are no dependent parameter types among parameters
  */
  def checkNoEscapeParams(vparams: Array[Array[Tree$ValDef]]): unit = {
    var i = 0; while (i < vparams.length) {
      var j = 0; while (j < vparams(i).length) {
	checkNoEscape(vparams(i)(j).pos, vparams(i)(j).tpe.getType());
	j = j + 1
      }
      i = i + 1
    }
  }

  /** Check that tree represents a pure definition.
  */
  def checkPureDef(tree: Tree, clazz: Symbol): unit = {
    if (!TreeInfo.isPureDef(tree) && tree.getType() != Type.ErrorType)
      error(tree.pos, "" + clazz + " may contain only pure definitions");
  }

  /** Check that tree represents a pure constructor.
  */
  def checkPureConstr(tree: Tree, clazz: Symbol): unit = {
    if (!TreeInfo.isPureConstr(tree) && tree.getType() != Type.ErrorType)
      error(tree.pos, "" + clazz + " may invoke only pure superclass constructors");
  }

  /** Check that tree represents a trait constructor.
  */
  def checkTrait(tree: Tree, clazz: Symbol): unit = {
    if (!tree.getType().symbol().isTrait() && tree.getType() != Type.ErrorType)
      error(tree.pos, " " + clazz + " may inherit only traits as mixins");
  }

  /** Check that tree is a stable expression .p
  */
  def checkStable(tree: Tree): Tree =
    if (TreeInfo.isPureExpr(tree) || tree.getType() == Type.ErrorType) tree;
    else error(tree.pos, "stable identifier required, but " + tree + " found.");

  /** Check that class can be instantiated.
  */
  def checkInstantiatable(pos: int, tp: Type): unit = {
    val clazz: Symbol = tp.symbol();
    if (clazz.kind == CLASS) {
      if (clazz.isAbstractClass())
	error(pos, "" + clazz + " is abstract, so it cannot be instantiated");
      else if (!tp.isSubType(tp.instanceType()))
	error(pos, "" + tp + " does not conform to its self-type " +
	      tp.instanceType() + ", so it cannot be instantiated");
    }
  }

  /** Check that all subtrees have their types defined.
  *  Used for asserting an internal invariant
  */
  private class CheckDefined extends Traverser {
    var all: Tree = null;
    override def traverse(tree: Tree): unit = {
      if (tree.getType() == null) assert(false, "" + tree + " in " + all);
      if (tree.getType() != Type.ErrorType)
	super.traverse(tree);
    }
  }

  private val checkDefined: CheckDefined = new CheckDefined();

  // Helper definitions for calculating types -----------------------------------------

  /** The qualifier type of a potential application of the `match' method.
  *  or NoType, if this is something else.
  */
  private def matchQualType(fn: Tree): Type = {
    fn match {
      case Tree$Select(qual, _) =>
	if (fn.symbol() == definitions.ANY_MATCH)
	  return qual.getType().widen();

      case Tree$TypeApply(fn1, _) =>
	return matchQualType(fn1);

      case Tree$Ident(_) =>
	if (fn.symbol() == definitions.ANY_MATCH)
	  return context.enclClass.owner.typeOfThis();

      case _ =>
    }
    if (fn.getType() == Type.ErrorType) Type.ErrorType else Type.NoType
  }

  private def isSetterMethod(sym: Symbol): boolean =
    sym != null &&
    !sym.isLocal() &&
    !sym.isStable() &&
    sym.getType().isInstanceOf[Type$PolyType] &&
    sym.typeParams().length == 0;

// Contexts -------------------------------------------------------------------

  /** Push new context associated with given tree, owner, and scope on stack.
  *  Fields `imports' and, possibly, `enclClass' are inherited from parent.
  */
  def pushContext(tree: Tree, owner: Symbol, scope: Scope): unit =
    context = new Context(tree, owner, scope, context);

  /** Pop context from stack.
  */
  def popContext(): unit =
    context = context.outer;

  // Lazy Types ------------------------------------------------------------------

  /** A lazy type which, when forced returns the type of a symbol defined
  *  in `tree'.
  */
  class LazyTreeType(_tree: Tree) extends Type$LazyType {
    val tree: Tree  = _tree;
    val u: Unit     = unit;
    val c: Context  = context;

    override def complete(sym: Symbol): unit = {
      defineSym(tree, u, c);
    }
  }

  /** A lazy type for case constructor methods (whose name is a term name)
  *  which sets the method's type to the class constructor type.
  */
  class LazyConstrMethodType(_tree: Tree) extends LazyTreeType(_tree) {
    override def complete(sym: Symbol): unit = {
      val constr: Symbol = tree.symbol().primaryConstructor();
      if (!sym.isInitialized())
	sym.setInfo(constr.getType().instanceType().cloneType(constr, sym));
    }
  }

  /** A lazy type for self types
  */
  class LazySelfType(clazz: Symbol, tree: Tree) extends LazyTreeType(tree) {
    override def complete(sym: Symbol): unit = {
      defineSelfType(sym, clazz, tree, u, c);
    }
  }

// Entering Symbols ----------------------------------------------------------

  def transformPackageId(tree: Tree): Tree = {
    if (tree.getType() != null) return tree;
    tree match {
      case Tree$Ident(name) =>
	tree
	.setSymbol(packageSymbol(tree.pos, context.owner, name))
	.setType(tree.symbol().getType())

      case Tree$Select(qual, name) =>
	val qual1: Tree = transformPackageId(qual);
	val sym: Symbol = packageSymbol(tree.pos, qual1.symbol(), name);
	copy.Select(tree, sym, qual1).setType(sym.getType())

      case _ =>
	transform(tree);
    }
  }

  def packageSymbol(pos: int, base: Symbol, name: Name): Symbol = {
    var p: Symbol = base.members().lookup(name);
    if (p.kind == NONE) {
      p = TermSymbol.newJavaPackageModule(name, base.moduleClass(), null);
      base.members().enter(p);
    } else if (!p.isPackage()) {
      error(pos, "package and class with same name");
      p = Symbol.ERROR;
    }
    p
  }

  /** Enter symbol `sym' in current scope. Check for double definitions.
  *  Handle overloading.
  */
  def enterInScope(sym: Symbol): unit = {
    // handle double and overloaded definitions
    val e: Scope$Entry = context.scope.lookupEntry(sym.name);
    val other: Symbol = e.sym;
    if (sym == other) {
      if (global.debug) global.log("redefined: " + sym + ":" + sym.rawInfo());
    } else if (e.owner == context.scope) {
      assert(!other.isExternal(), other);
      if (sym.owner().isPackage()) {
	if (global.compiledNow.get(other) != null) {
	  error(sym.pos, "" + sym + " is compiled twice");
	}
	context.scope.unlink(e);
	context.scope.enter(sym);
      } else if (context.owner.kind == CLASS && sym.kind == VAL && other.kind == VAL && ((sym.flags & ACCESSOR) == 0 || (other.flags & ACCESSOR) == 0)) {
		    e.setSymbol(other.overloadWith(sym));
       } else {
	 if (context.owner.kind == CLASS)
	   error(sym.pos,
		 sym.nameString() + " is already defined as " +
		 other + other.locationString());
	 else
	   error(sym.pos,
		 sym.nameString() +
		 " is already defined in local scope");
       }
    } else {
      context.scope.enter(sym);
    }
    if (sym.owner().isPackage())
      global.compiledNow.put(sym, unit.source);
  }

  /** If `tree' is a definition, create a symbol for it with a lazily
  *  constructed type, and enter into current scope.
  */
  def enterSym(tree: Tree): Symbol = {

    /** Enter `sym' in current scope and make it the symbol of `tree'.
    */
    def enterSym(tree: Tree, sym: Symbol): Symbol = {
      //if (global.debug) System.out.println("entering " + sym);//DEBUG
      if (!sym.isInitialized()) {
	//System.err.println("undefined: " + sym + ":" + sym.rawInfo());//DEBUG
	sym.setInfo(new LazyTreeType(tree));
      }
      if (!sym.isConstructor()) {
	val owner: Symbol = sym.owner();
	if (sym.kind == VAL && (sym.flags & (PRIVATE | SEALED)) == 0 &&
	    owner != null && owner.kind == CLASS &&
	    (owner.flags & FINAL) != 0)
	      sym.flags =  sym.flags | FINAL;
	enterInScope(sym);
      }
      tree.setSymbol(sym);

      // set the comment associated with a symbol
      val comment: String = global.mapTreeComment.get(tree).asInstanceOf[String];
      if (comment != null)
	global.mapSymbolComment.put(sym, comment);

      sym
    }

    /** Make `sym' the symbol of import `tree' and create an entry in
    *  current imports list.
    */
    def enterImport(tree: Tree, sym: Symbol): Symbol = {
      sym.setInfo(new LazyTreeType(tree));
      tree.setSymbol(sym);
      context.imports = new ImportList(tree, context.scope, context.imports);
      sym
    }

    val owner: Symbol = context.owner;
    tree match {
      case Tree$PackageDef(_packaged, templ) =>
	var packaged = _packaged;
	templ match {
	  case Tree$Template(_, body) =>
	    pushContext(tree, context.owner, context.scope);
	    context.imports = null;
	    packaged = transformPackageId(packaged);
	    tree.asInstanceOf[Tree$PackageDef].packaged = packaged;
	    popContext();
	    val pkg: Symbol = checkStable(packaged).symbol();
	    if (pkg != null && pkg.kind != ERROR) {
	      if (pkg.isPackage()) {
		pushContext(templ, pkg.moduleClass(), pkg.members());
		enterSyms(body);
		popContext();
	      } else {
		error(tree.pos, "only Java packages allowed for now");
	      }
	    }
	    templ.setSymbol(Symbol.NONE);
	    null
	  case _ =>
	    throw new ApplicationError();
	}

      case Tree$ClassDef(mods, name, tparams, vparams, _, templ) =>
	val clazz: ClassSymbol = ClassSymbol.define(
	  tree.pos, name, owner, mods, context.scope);
	if (clazz.isLocalClass()) unit.mangler.setMangledName(clazz);
	if (!clazz.primaryConstructor().isInitialized())
	  clazz.primaryConstructor().setInfo(new LazyTreeType(tree));
	if ((mods & CASE) != 0) {
	  if ((mods & ABSTRACT) == 0) {
	    // enter case constructor method.
	    val cf: Symbol = TermSymbol.define(
	      tree.pos, name.toTermName(), owner,
	      mods & ACCESSFLAGS | CASE, context.scope);
	    enterInScope(cf);
	    if (!cf.isInitialized() || cf.info().symbol().isModuleClass()) {
	      cf.setInfo(new LazyConstrMethodType(tree));
	    }
	  }
	}
	enterSym(tree, clazz)

      case Tree$ModuleDef(mods, name, _, _) =>
	val modul: TermSymbol = TermSymbol.define(
	  tree.pos, name, owner, mods, context.scope).makeModule();
	val clazz: Symbol = modul.moduleClass();
	if (!clazz.isInitialized())
	  clazz.setInfo(new LazyTreeType(tree));
	if (clazz.isLocalClass()) unit.mangler.setMangledName(clazz);
	enterSym(tree, modul)

      case Tree$ValDef(mods, name, _, _) =>
	enterSym(
	  tree,
	  TermSymbol.define(tree.pos, name, owner, mods, context.scope))

      case Tree$DefDef(mods, name, _, _, _, _) =>
	var sym: Symbol = null;
	if (name == Names.CONSTRUCTOR) {
	    val clazz: Symbol = context.enclClass.owner;
	    if (!(context.tree.isInstanceOf[Tree$Template]) ||
		clazz.isModuleClass() ||
		clazz.isAnonymousClass() ||
		clazz.isCompoundSym() ||
		clazz.isPackage()) {
	       error(tree.pos, "constructor definition not allowed here");
	    }
	    sym = context.enclClass.owner.addConstructor();
	    sym.flags = sym.flags | mods;
	} else {
	  sym = TermSymbol.define(tree.pos, name, owner, mods, context.scope);
	}
	enterSym(tree, sym);

      case Tree$AliasTypeDef(mods, name, _, _) =>
	val tsym: Symbol = AliasTypeSymbol.define(tree.pos, name, owner, mods, context.scope);
	if (!tsym.primaryConstructor().isInitialized())
	  tsym.primaryConstructor().setInfo(new LazyTreeType(tree));
	enterSym(tree, tsym)

      case Tree$AbsTypeDef(mods, name, _, _) =>
	enterSym(
	  tree,
	  AbsTypeSymbol.define(tree.pos, name, owner, mods, context.scope))

      case Tree$Import(expr, selectors) =>
	enterImport(tree,
		    new TermSymbol(
		      tree.pos,
		      Name.fromString("import " + expr),
		      Symbol.NONE, SYNTHETIC))

      case _ =>
	null
    }
  }

  /** Enter all symbols in statement list
  */
  def enterSyms(stats: Array[Tree]): unit = {
    var i = 0; while (i < stats.length) {
      enterSym(stats(i));
      i = i + 1
    }
  }

  def enterParams[t <: Tree](params: Array[t]): Array[Symbol] = {
    var i = 0; while (i < params.length) {
      enterSym(params(i));
      (params(i) : Tree) match {
	case Tree$ValDef(mods, _, _, _) =>
	  if ((mods & REPEATED) != 0 && i != params.length - 1)
	    error(params(i).pos,
		  "`*' parameter must be the last parameter of a `('...`)' section");
	case _ =>
      }
      i = i + 1
    }
    Tree.symbolOf(params.asInstanceOf[Array[Tree]])
  }

  def enterParams(vparams: Array[Array[Tree$ValDef]]): Array[Array[Symbol]] = {
    val vparamSyms = new Array[Array[Symbol]](vparams.length);
    var i = 0; while (i < vparams.length) {
      vparamSyms(i) = enterParams(vparams(i));
      i = i + 1
    }
    vparamSyms
  }

  /** Re-enter type parameters in current scope.
  */
  def reenterParams(tparams: Array[Tree$AbsTypeDef], tsyms: Array[Symbol]): unit = {
    var i = 0; while (i < tparams.length) {
      tsyms(i).pos = tparams(i).pos;
      tsyms(i).name = tparams(i).name;
      //necessary since tsyms might have been unpickled
      tparams(i).setSymbol(tsyms(i));
      context.scope.enter(tsyms(i));
      i = i + 1
    }
  }

  /** Re-enter type and value parameters in current scope.
  */
  def reenterParams(tparams: Array[Tree$AbsTypeDef], vparamss: Array[Array[Tree$ValDef]], mt: Type): unit = {
    var rest: Type = mt;
    rest match {
      case Type$PolyType(tsyms, restp) =>
	reenterParams(tparams, tsyms);
	rest = restp;
      case _ =>
    }
    var j = 0; while (j < vparamss.length) {
      val vparams = vparamss(j);
      rest match {
	case Type$MethodType(vsyms, restp) =>
	  var i = 0; while (i < vparams.length) {
	    vsyms(i).pos = vparams(i).pos;
	    vsyms(i).name = vparams(i).name;
            //necessary since vsyms might have been unpickled
	    vparams(i).setSymbol(vsyms(i));
	    context.scope.enter(vsyms(i));
	    i = i + 1
	  }
	  rest = restp;
	case _ =>
      }
      j = j + 1
    }
  }

// Definining Symbols -------------------------------------------------------

  /** Define symbol associated with `tree' using given `unit' and `context'.
  */
  def defineSym(tree: Tree, unit: Unit, curcontext: Context): unit = {
    val savedUnit: Unit = this.unit;
    this.unit = unit;
    val savedContext: Context = this.context;
    this.context = curcontext;
    val savedMode: int = this.mode;
    this.mode = EXPRmode;
    val savedPt: Type = this.pt;
    this.pt = Type.AnyType;

    try {
      val sym: Symbol = tree.symbol();
      if (global.debug) global.log("defining " + sym);
      var owntype: Type = null;
      tree match {
	case Tree$ClassDef(mods, name, tparams, vparams, tpe, templ) =>
	  pushContext(
	    tree, sym.primaryConstructor(), new Scope(context.scope));
	  val tparamSyms = enterParams(tparams);
	  var vparamSyms = enterParams(vparams);
	  if (vparamSyms.length == 0)
	    vparamSyms = NewArray.SymbolArray{Symbol.EMPTY_ARRAY};
	  if ((mods & CASE) != 0 && vparams.length > 0)
	    templ.body = desugarize.addCaseElements(templ.body, vparams(0));

	  val constrtype: Type = makeMethodType(
	    tparamSyms,
	    vparamSyms,
	    new Type$TypeRef(sym.owner().thisType(), sym, Symbol.getType(tparamSyms)));
	  //System.out.println("set info " + sym.constructor() + " to " + constrtype + " was " + sym.constructor().rawInfo());//DEBUG
	  sym.primaryConstructor().setInfo(constrtype);
	  // necessary so that we can access tparams
	  sym.primaryConstructor().flags =
	    sym.primaryConstructor().flags | INITIALIZED;

	  if (tpe != Tree.Empty)
	    sym.setTypeOfThis(new LazySelfType(sym, tpe));

	  defineTemplate(templ, sym, new Scope());
	  owntype = templ.getType();
	  popContext();

	case Tree$ModuleDef(mods, name, _tpe, templ) =>
	  var tpe = _tpe;
	  val clazz: Symbol = sym.moduleClass();
	  defineTemplate(templ, clazz, new Scope());
	  clazz.setInfo(templ.getType());
	  tpe = transform(tpe, TYPEmode);
	  (tree.asInstanceOf[Tree$ModuleDef]).tpe = tpe;
	  if (tpe != Tree.Empty)
	    clazz.setTypeOfThis(new LazySelfType(sym, tpe));
	  owntype = if (tpe == Tree.Empty) clazz.getType() else tpe.getType();

	case Tree$DefDef(mods, name, tparams, vparams, _tpe, _rhs) =>
	  var tpe = _tpe;
	  var rhs = _rhs;
	  var restype: Type = null;
	  pushContext(tree, sym, new Scope(context.scope));
	  if (name == Names.CONSTRUCTOR)
	    context.enclClass.owner.flags =
	      context.enclClass.owner.flags | INCONSTRUCTOR;
	  val tparamSyms = enterParams(tparams);
	  val vparamSyms = enterParams(vparams);
	  if (tpe != Tree.Empty) {
	    tpe = transform(tpe, TYPEmode);
	    (tree.asInstanceOf[Tree$DefDef]).tpe = tpe;
	    restype = tpe.getType();
	  } else if (name == Names.CONSTRUCTOR) {
	    restype = context.enclClass.owner.getType().subst(
	      context.enclClass.owner.typeParams(), tparamSyms);
	      context.enclClass.owner.flags =
		context.enclClass.owner.flags & ~INCONSTRUCTOR;
	  } else {
	    rhs = transform(rhs, EXPRmode);
	    (tree.asInstanceOf[Tree$DefDef]).rhs = rhs;
	    restype = rhs.getType();
	    if (!sym.isFinal())	restype = restype.deconst();
	  }
	  restype = checkNoEscape(tpe.pos, restype);
	  popContext();
	  owntype = makeMethodType(tparamSyms, vparamSyms, restype);
	  //System.out.println("methtype " + name + ":" + owntype);//DEBUG

	case Tree$ValDef(mods, name, _tpe, _rhs) =>
	  var tpe = _tpe;
	  var rhs = _rhs;
	  if (tpe != Tree.Empty) {
	    tpe = transform(tpe, TYPEmode);
	    (tree.asInstanceOf[Tree$ValDef]).tpe = tpe;
	    owntype = tpe.getType();
	  } else {
	    pushContext(tree, sym, context.scope);
	    if (rhs == Tree.Empty) {
	      if ((sym.owner().flags & ACCESSOR) != 0) {
		// this is the parameter of a variable setter method.
		assert((sym.flags & PARAM) != 0);
		owntype = sym.owner().accessed().getType();
	      } else {
		error(tree.pos, "missing parameter type");
		owntype = Type.ErrorType;
	      }
	    } else {
	      if ((sym.flags & CASEACCESSOR) != 0) {
		rhs.setType(rhs.symbol().getType());
	      } else {
		rhs = transform(rhs, EXPRmode);
		(tree.asInstanceOf[Tree$ValDef]).rhs = rhs;
	      }
	      owntype = rhs.getType();
	      if (sym.isVariable() || !sym.isFinal())
		owntype = owntype.deconst();
	    }
	    popContext();
	  }

	case Tree$AliasTypeDef(mods, name, tparams, _rhs) =>
	  var rhs = _rhs;
	  pushContext(tree, sym.primaryConstructor(), new Scope(context.scope));
	  val tparamSyms = enterParams(tparams);
	  sym.primaryConstructor().setInfo(
	    new Type$PolyType(tparamSyms, sym.typeConstructor()));
	  // necessary so that we can access tparams
	  sym.primaryConstructor().flags =
	    sym.primaryConstructor().flags | INITIALIZED;
	  rhs = transform(rhs, TYPEmode);
	  (tree.asInstanceOf[Tree$AliasTypeDef]).rhs = rhs;
	  owntype = rhs.getType();
	  popContext();

	case Tree$AbsTypeDef(mods, name, _rhs, _lobound) =>
	  var rhs = _rhs;
	  var lobound = _lobound;
	  //can't have `sym' as owner since checkNonCyclic would fail.
	  rhs = transform(rhs, TYPEmode);
	  tree.asInstanceOf[Tree$AbsTypeDef].rhs = rhs;
	  lobound = transform(lobound, TYPEmode);
	  (tree.asInstanceOf[Tree$AbsTypeDef]).lobound = lobound;
	  owntype = rhs.getType();
	  sym.setLoBound(lobound.getType());
	  owntype.symbol().initialize();//to detect cycles todo: needed?

	case Tree$Import(_expr, selectors) =>
	  val expr  = transform(_expr, EXPRmode | QUALmode);
	  tree.asInstanceOf[Tree$Import].expr = expr;
	  checkStable(expr);
	  owntype = expr.getType();
	  val tp: Type = owntype.widen();
	  var i = 0; while (i < selectors.length) {
	    if (selectors(i) != Names.IMPORT_WILDCARD &&
		tp.lookup(selectors(i)) == Symbol.NONE &&
		tp.lookup(selectors(i).toTypeName()) == Symbol.NONE)
	      error(tree.pos, "" + NameTransformer.decode(selectors(i)) + " is not a member of " + expr);
	    i = i + 2
	  }

	case _ =>
	  throw new ApplicationError();
      }
      sym.setInfo(owntype);
      validate(sym);
      if (global.debug) global.log("defined " + sym);
    } catch {
      case ex: Type$Error =>
	reportTypeError(tree.pos, ex);
	tree.setType(Type.ErrorType);
	if (tree.hasSymbol()) {
	  if (tree.symbol() != null) tree.symbol().setInfo(Type.ErrorType);
	  else tree.setSymbol(Symbol.ERROR);
	}
    }

    this.unit = savedUnit;
    this.context = savedContext;
    this.mode = savedMode;
    this.pt = savedPt;
  }

  /** Definition phase for a template. This enters all symbols in template
  *  into symbol table.
  */
  def defineTemplate(templ: Tree$Template, clazz: Symbol, members: Scope): unit = {
    // attribute parent constructors
    val constrs = transformConstrInvocations(templ.pos, templ.parents);
    val parents = Tree.typeOf(constrs);

    // enter all members
    pushContext(templ, clazz, members);
    templ.body = desugarize.Statements(templ.body, false);
    enterSyms(templ.body);
    popContext();
    templ.setType(Type.compoundType(parents, members, clazz));
  }

  def makeMethodType(tparams: Array[Symbol], vparams: Array[Array[Symbol]], restpe: Type): Type =
    if (tparams.length == 0 && vparams.length == 0) {
      new Type$PolyType(tparams, restpe);
    } else {
      var result: Type = restpe;
      var i = vparams.length - 1;
      while (i >= 0) {
	result = new Type$MethodType(vparams(i), result);
	i = i - 1;
      }
      if (tparams.length != 0)
	result = new Type$PolyType(tparams, result);
      result
    }

  def makeStableId(pos: int, tp: Type): Tree =
    if (tp.symbol().isCompoundSym()) gen.This(pos, tp.symbol())
    else gen.mkStableId(pos, tp);

  /** Define self type of class or module `sym'
  *  associated with `tree' using given `unit' and `context'.
  */
  def defineSelfType(sym: Symbol, clazz: Symbol, tree: Tree, unit: Unit, curcontext: Context): unit = {
    val savedUnit: Unit = this.unit;
    this.unit = unit;
    val savedContext: Context = this.context;
    this.context = curcontext;

    val selftype: Type = transform(tree, TYPEmode).getType();
    //todo: can we really make a compound type with class
    val selftype1: Type =
      if (clazz.getType().isSubType(selftype))
	clazz.getType()
      else if (selftype.isSubType(clazz.getType()))
	selftype
      else
	selftype match {
	  case Type$CompoundType(parts, members) =>
	    val parts1 = new Array[Type](parts.length + 1);
	    System.arraycopy(parts, 0, parts1, 0, parts.length);
	    parts1(parts.length) = clazz.getType();
	    Type.compoundType(parts1, members);

	  case _ =>
	    Type.compoundType(
	      NewArray.Type(selftype, clazz.getType()), Scope.EMPTY);
	}
    sym.setInfo(selftype1);

    this.unit = savedUnit;
    this.context= savedContext;
  }

// Attribution and Transform -------------------------------------------------

  /** Turn tree type into stable type if possible and required by
  *  context.
  */
  def mkStable(tree: Tree, pre: Type, mode: int, pt: Type): Tree = {
    tree.getType() match {
      case Type$ConstantType(_, value) =>
	return make.Literal(tree.pos, value).setType(tree.getType())
      case Type$PolyType(tparams, restp) =>
	if (tparams.length == 0) {
	  restp match {
	    case Type$ConstantType(_, value) =>
	      return make.Literal(tree.pos, value).setType(tree.getType());
	    case _ =>
	  }
	}
      case _ =>
    }
    if ((pt != null && pt.isStable() || (mode & QUALmode) != 0) && pre.isStable()) {
      var sym: Symbol = tree.symbol();
      tree.getType() match {
	case Type$OverloadedType(alts, alttypes) =>
	  if ((mode & FUNmode) == 0) {
	    try {
	      infer.exprAlternative(tree, alts, alttypes, pt);
	      sym = tree.symbol();
	    } catch {
	      case ex: Type$Error =>
		reportTypeError(tree.pos, ex);
	    }
	  }
	case _ =>
      }
      if (sym.isStable()) {
	tree.setType(Type.singleType(pre, sym));
      }
    }
    tree
  }

  /** Adapt tree to given mode and given prototype
  */
  def adapt(tree: Tree, mode: int, pt: Type): Tree = {
    //System.out.println(tree + ":" + tree.getType() + " adapt " + pt + " " + mode);//DEBUG
    tree.getType() match {
      case Type$OverloadedType(alts, alttypes) =>
	// resolve overloading
	if ((mode & FUNmode) == 0) {
	  try {
	    infer.exprAlternative(tree, alts, alttypes, pt);
	  } catch {
	    case ex: Type$Error =>
	      reportTypeError(tree.pos, ex);
	  }
	  tree.getType() match {
	    case Type$OverloadedType(_, _) =>
	      // overload resolution failed bcs no alternative matched prototype.
	      typeError(tree.pos, tree.getType(), pt);
	      tree.setSymbol(Symbol.ERROR).setType(Type.ErrorType);
	    case _ =>
	  }
	  return adapt(tree, mode, pt);
	}

      case Type$PolyType(tparams, restp) =>
	// apply parameterless functions
	// instantiate polymorphic expressions
	if (tparams.length == 0) {
	  return adapt(tree.setType(restp), mode, pt);
	} else if ((mode & (FUNmode | POLYmode)) == 0) {
	  var tree1: Tree = null;
	  try {
	    tree1 = infer.exprInstance(tree, tparams, restp, pt);
	  } catch {
	    case ex: Type$Error =>
	      tree1 = error(tree.pos, ex.msg);
	  }
	  return adapt(tree1, mode, pt);
	}

      case Type$MethodType(_, _) =>
	// convert unapplied methods to functions.
	if ((mode & (EXPRmode | FUNmode)) == EXPRmode &&
	    (infer.isCompatible(tree.getType(), pt) ||
	     pt.symbol() == definitions.UNIT_CLASS)) {
	  checkEtaExpandable(tree.pos, tree.getType());
	  return transform(desugarize.etaExpand(tree, tree.getType()), mode, pt);
	} else if ((mode & (CONSTRmode | FUNmode)) == CONSTRmode) {
	  return error(tree.pos, "missing arguments for class constructor");
	}

      case _ =>
    }
    if ((mode & PATTERNmode) != 0) {
      if (tree.isType()) {
	val clazz: Symbol = tree.getType().withDefaultArgs().unalias().symbol();

	if (clazz.isCaseClass()) {
	  // set type to instantiated case class constructor
	  tree.setType(tree.getType().prefix().memberType(clazz.primaryConstructor()));
	  // MZ: this is a hack, but I didn't know how to do it better
	  if ((clazz.flags & (JAVA | CASE)) == (JAVA | CASE)) {
	    val altconstr = clazz.allConstructors().alternativeSymbols();
	    tree.setType(tree.getType().prefix().memberType(
	      altconstr(altconstr.length - 1)));
	  }
	  tree.getType() match {
	    case Type$PolyType(tparams, restp) =>
	      try {
		infer.constructorInstance(tree, tparams, restp, pt);
		//System.out.println("constr inst " + ArrayApply.toString(tparams) + restp + " against " + pt + " = " + tree.getType());//DEBUG
	      } catch {
		case ex: Type$Error =>
		  if (pt != Type.ErrorType) error(tree.pos, ex.msg);
		  tree.setType(Type.ErrorType);
	      }
	    case _ =>
	  }
	} else if (clazz.isSubClass(definitions.SEQ_CLASS)) {
	  // set type to instantiated sequence class constructor
	  // todo: should we admit even supertypes of the target type?
	  val seqtp: Type = pt.baseType(clazz);
	  if (seqtp != Type.NoType) {
	    def seqConstructorType(paramtp: Type, resulttp: Type) = {
	      val constr: Symbol = resulttp.symbol().primaryConstructor();
	      val param: Symbol = new TermSymbol(
		Position.NOPOS, Names.PATTERN_WILDCARD, constr, PARAM | REPEATED).setInfo(
		  paramtp.baseType(definitions.SEQ_CLASS));
	      new Type$MethodType(NewArray.Symbol(param), resulttp);
	    }
	    tree.setType(seqConstructorType(seqtp, pt));
	  } else {
	    return error(tree.pos, "expected pattern type " + pt +
			 " does not conform to sequence " + clazz);
	  }
	} else if (tree.getType() != Type.ErrorType) {
	  return error(tree.pos, "" + tree.getType().symbol() +
		       " is neither a case class constructor nor a sequence class constructor");
	}
      }
      if ((mode & FUNmode) != 0) {
	return tree;
      } else {
	val sym: Symbol = tree.symbol();
	// check that idents or selects are stable.
	tree match {
	  case Tree$Ident(_) | Tree$Select(_, _) =>
	    checkStable(tree);
	  case _ =>
	}
      }
    } else if ((mode & EXPRmode) != 0) {
      if ((mode & FUNmode) != 0) {
	if (tree.getType().isObjectType()) {
	  // insert apply method
	  val applyMeth: Symbol = tree.getType().lookup(Names.apply);
	  if (applyMeth != Symbol.NONE) {
	    val applyType: Type = checkAccessible(
	      tree.pos, applyMeth, tree.getType().memberType(applyMeth),
	      tree, tree.getType());
	    val tree1 = make.Select(tree.pos, tree, Names.apply)
	      .setSymbol(applyMeth)
	      .setType(applyType);
	    return adapt(tree1, mode, pt);
	  }
	}
      } else if ((mode & QUALmode) == 0) {
	// check that packages and static modules are not used as values
        tree match {
          case Tree$Ident(_) | Tree$Select(_, _) =>
            val sym: Symbol = tree.symbol();
            if (sym != null && sym.kind != ERROR && !sym.isValue()) {
              return error(tree.pos, "" + tree.symbol() + " is not a value");
            }
	  case _ =>
        }
      }
    }

    var owntype: Type = tree.getType();
    if ((mode & (CONSTRmode | FUNmode)) == (CONSTRmode)) {
      owntype = owntype.instanceType();
      // this works as for superclass constructor calls the expected
      // type `pt' is always AnyType (see transformConstrInvocations).
    }
    if (!(owntype.isInstanceOf[Type$PolyType] || owntype.isSubType(pt))) {
      tree match {
	case Tree$Literal(value) =>
	  var n: int = Integer.MAX_VALUE;
	  if (value.isInstanceOf[Integer])
	    n = value.asInstanceOf[Integer].intValue();
	  else if (value.isInstanceOf[Character])
	    n = value.asInstanceOf[Character].charValue();
	  val value1: Object =
	    if (pt.symbol() == definitions.BYTE_CLASS && -128 <= n && n <= 127)
	      new Byte(n.asInstanceOf[byte])
	    else if (pt.symbol() == definitions.SHORT_CLASS && -32768 <= n && n <= 32767)
	      new Short(n.asInstanceOf[short])
	    else if (pt.symbol() == definitions.CHAR_CLASS && 0 <= n && n <= 65535)
	      new Character(n.asInstanceOf[char])
	    else null;
	  if (value1 != null)
	    return make.Literal(tree.pos, value1).setType(new Type$ConstantType(pt, value1));
	case _ =>
      }
      if ((mode & EXPRmode) != 0) {
	if (pt.symbol() == definitions.UNIT_CLASS) {
	  return gen.Block(NewArray.Tree(tree, gen.mkUnitLit(tree.pos)));
	} else if (infer.isCompatible(tree.getType(), pt)) {
	  val coerceMeth: Symbol = tree.getType().lookup(Names.coerce);
	  if (coerceMeth != Symbol.NONE) {
	    val coerceType: Type = checkAccessible(
	      tree.pos, coerceMeth, tree.getType().memberType(coerceMeth),
	      tree, tree.getType());
	    val tree1 = make.Select(tree.pos, tree, Names.coerce)
	    .setSymbol(coerceMeth)
	    .setType(coerceType);
	    return adapt(tree1, mode, pt);
	  }
	}
      }
      if ((mode & CONSTRmode) == 0) {
	typeError(tree.pos, owntype, pt);
	Type.explainTypes(owntype, pt);
	tree.setType(Type.ErrorType);
      } // for constructors, delay until after the `new'.
    }
    tree
  }

  /** Attribute an identifier consisting of a simple name or an outer reference.
  *  @param tree      The tree representing the identifier.
  *  @param name      The name of the identifier.
  */
  def transformIdent(_tree: Tree, name: Name): Tree = {
    var tree = _tree;
    //System.out.println("transforming " + name);//DEBUG
    // find applicable definition and assign to `sym'
    var sym: Symbol = Symbol.NONE;
    var pre: Type = null;
    var qual: Tree = Tree.Empty;
    var stopPos: int = Integer.MIN_VALUE;
    var nextcontext: Context = context;
    while (sym.kind == NONE && nextcontext != Context.NONE) {
      sym = nextcontext.scope.lookup(name);
      if (sym.kind != NONE) {
	stopPos = sym.pos;
      } else {
	nextcontext = nextcontext.enclClass;
	if (nextcontext != Context.NONE) {
	  sym = nextcontext.owner.thisSym().info().lookup(name);
	  if (sym.kind != NONE) {
	    stopPos = nextcontext.owner.pos;
	  } else {
	    nextcontext = nextcontext.outer;
	  }
	}
      }
    }

    // find applicable import and assign to `sym1'
    var nextimports: ImportList = context.imports;
    var lastimports: ImportList = null;
    var sym1: Symbol = Symbol.NONE;

//	System.out.println("name = " + name + ", pos = " + tree.pos + ", importlist = ");//DEBUG
//	for (val imp: ImportList = nextimports; imp != null; imp = imp.prev) {
//	    new TextTreePrinter().print("    ").print(imp.tree).println().end();//DEBUG
//	}

    while (nextimports != null && nextimports.tree.pos >= tree.pos) {
      nextimports = nextimports.prev;
    }
    while (sym1.kind == NONE && nextimports != null && nextimports.tree.pos > stopPos) {
      sym1 = nextimports.importedSymbol(name);
      lastimports = nextimports;
      nextimports = nextimports.prev;
    }

    // evaluate what was found
    if (sym1.kind == NONE) {
      if (sym.kind == NONE) {
	//System.out.println(name);//DEBUG
	error(tree.pos, "not found: " + decode(name));
	return tree.setSymbol(Symbol.ERROR).setType(Type.ErrorType);
      } else {
	if (sym.owner().kind == CLASS) {
	  pre = nextcontext.enclClass.owner.thisType();
	  if (!sym.owner().isPackage()) {
	    val qual1: Tree = makeStableId(tree.pos, pre);
	    tree = make.Select(tree.pos, qual1, name);
	    //System.out.println(name + " :::> " + tree + " " + qual1.symbol());//DEBUG
	  }
	} else {
	  pre = Type.localThisType;
	}
      }
    } else if (sym.kind != NONE && !sym.isExternal()) {
      return error(tree.pos,
		   "reference to " + name + " is ambiguous;\n" +
		   "it is both defined in " + sym.owner() +
		   " and imported subsequently by \n" + lastimports.tree);
    } else {
      // check that there are no other applicable imports in same scope.
      while (nextimports != null && nextimports.enclScope == lastimports.enclScope) {
	if (!nextimports.sameImport(lastimports) && nextimports.importedSymbol(name).kind != NONE) {
	  return error(tree.pos,
		       "reference to " + name + " is ambiguous;\n" +
		       "it is imported twice in the same scope by\n    " +
		       lastimports.tree + "\nand " + nextimports.tree);
	}
	nextimports = nextimports.prev;
      }
      sym = sym1;
      qual = lastimports.importPrefix().duplicate();
      pre = qual.getType();
      //new TextTreePrinter().print(name + " => ").print(lastimports.tree).print("." + name).println().end();//DEBUG
      tree = make.Select(tree.pos, qual, name);
    }

    var symtype: Type =
      (if (sym.isType()) sym.typeConstructor() else sym.getType())
      .asSeenFrom(pre, sym.owner());
    if (qual != Tree.Empty)
      symtype = checkAccessible(tree.pos, sym, symtype, qual, qual.getType());
    else if (sym.owner().isPackage())
      symtype = checkAccessible(tree.pos, sym, symtype, qual, sym.owner().getType());
    if (symtype == Type.NoType)
      return error(tree.pos, "not found: " + decode(name));
    //System.out.println(name + ":" + symtype);//DEBUG
    mkStable(tree.setSymbol(sym).setType(symtype), pre, mode, pt)
  }

  /** Attribute a selection where `tree' is `qual.name'.
  *  `qual' is already attributed.
  */
  def transformSelect(tree: Tree, _qual: Tree, name: Name): Tree = {
    var qual = _qual;
    var uninst: Array[Symbol] = Symbol.EMPTY_ARRAY;
    qual.getType() match {
      case Type$PolyType(tparams, restype) =>
	qual = infer.mkTypeApply(qual, tparams, restype, Symbol.getType(tparams));
	uninst = tparams;
      case _ =>
    }
    val sym: Symbol = qual.getType().lookup(name);
    if (sym.kind == NONE) {
      //System.out.println(qual.getType() + " has members " + qual.getType().members());//DEBUG
      error(tree.pos,
	    decode(name) + " is not a member of " + qual.getType().widen());
    } else {
      val qualtype =
	if (qual.isInstanceOf[Tree$Super]) context.enclClass.owner.thisType()
	else qual.getType();
      var symtype: Type =
	(if (sym.isType()) sym.typeConstructor() else sym.getType())
	.asSeenFrom(qualtype, sym.owner());
      if (symtype == Type.NoType)
	return error(tree.pos, "not found: " + decode(name));
      else
	symtype = checkAccessible(tree.pos, sym, symtype, qual, qualtype);
      //System.out.println(sym.name + ":" + symtype);//DEBUG
      if (uninst.length != 0) {
	symtype match {
	  case Type$PolyType(tparams, restype) =>
	    symtype = new Type$PolyType(tparams, new Type$PolyType(uninst, restype));
	  case _ =>
	    symtype = new Type$PolyType(uninst, symtype);
	}
      }
      //System.out.println(qual.getType() + ".member: " + sym + ":" + symtype);//DEBUG
      val tree1: Tree = tree match {
	case Tree$Select(_, _) =>
	  copy.Select(tree, sym, qual)
	case Tree$SelectFromType(_, _) =>
	  copy.SelectFromType(tree, sym, qual)
      }
      mkStable(tree1.setType(symtype), qualtype, mode, pt)
    }
  }

  /** Attribute a pattern matching expression where `pattpe' is the
  *  expected type of the patterns and `pt' is the expected type of the
  *  results.
  */
  def transformVisitor(tree: Tree, pattpe: Type, pt: Type): Tree =
    //System.out.println("trans visitor with " + pattpe + "," + pt);//DEBUG
    tree match {
      case Tree$Visitor(cases) =>
	val cases1 = cases;
	var i = 0; while (i < cases.length) {
	  cases1(i) = transformCase(cases(i), pattpe, pt);
	  i = i + 1
	}
	return copy.Visitor(tree, cases1)
	  .setType(Type.lub(Tree.typeOf(cases1.asInstanceOf[Array[Tree]])));
      case _ =>
	throw new ApplicationError();
    }

  /** Attribute a case where `pattpe' is the expected type of the pattern
  *  and `pt' is the expected type of the result.
  */
  def transformCase(tree: Tree$CaseDef, pattpe: Type, pt: Type): Tree$CaseDef =
    tree match {
      case Tree$CaseDef(pat, guard, body) =>
	pushContext(tree, context.owner, new Scope(context.scope));
	this.inAlternative = false;       // no vars allowed below Alternative
	val pat1: Tree = transform(pat, PATTERNmode, pattpe);
	val guard1: Tree =
	  if (guard == Tree.Empty) Tree.Empty
	  else transform(guard, EXPRmode, definitions.BOOLEAN_TYPE());
	val body1: Tree = transform(body, EXPRmode, pt);
	popContext();
	return copy.CaseDef(tree, pat1, guard1, body1)
	  .setType(body1.getType()).asInstanceOf[Tree$CaseDef];
    }

  def transformStatSeq(stats: Array[Tree], exprOwner: Symbol): Array[Tree] = {
    var stats1 = stats;
    var i = 0; while (i < stats.length) {
      val stat: Tree = stats(i);
      if (context.owner.isCompoundSym() && !TreeInfo.isDeclaration(stat)) {
	error(stat.pos, "only declarations allowed here");
      }
      val mode: int = if (TreeInfo.isDefinition(stat)) NOmode else EXPRmode;
      var stat1: Tree = null;
      if (exprOwner.kind != NONE && !TreeInfo.isDefinition(stat)) {
	pushContext(stat, exprOwner, context.scope);
	stat1 = transform(stat, mode);
	popContext();
      } else {
	stat1 = transform(stat, mode);
      }
      // todo: if we comment next 4 lines out, test/pos/scoping2 fails.
      // find out why
      if (stat1 != stat && stats1 == stats) {
	stats1 = new Array[Tree](stats.length);
	System.arraycopy(stats, 0, stats1, 0, i);
      }
      stats1(i) = stat1;
      i = i + 1
    }
    stats1
  }

  /** Attribute a sequence of constructor invocations.
  */
  def transformConstrInvocations(pos: int, constrs: Array[Tree]): Array[Tree] = {
    var i = 0; while (i < constrs.length) {
      constrs(i) = transform(constrs(i), CONSTRmode | SUPERmode, Type.AnyType);
      val f: Symbol = TreeInfo.methSymbol(constrs(i));
      if (f != null) {
	val c: Symbol = f.constructorClass();
	if (c.kind == CLASS) {
	  c.initialize();//to detect cycles
	  if (i > 0 && (c.flags & JAVA) == 0 && c.isExternal()) {
	    // need to load tree for mixins
	    new SourceCompleter(global, true).complete(c);
	  }
	}
      }
      i = i + 1
    }
    constrs
  }

  def transformConstrInvocationArgs(constrs: Array[Tree]): unit = {
    var i = 0; while (i < constrs.length) {
      constrs(i) match {
	case Tree$Apply(fn, args) =>
	  if (fn.getType().isInstanceOf[Type$MethodType])
	    transformArgs(
	      constrs(i).pos, TreeInfo.methSymbol(fn), Symbol.EMPTY_ARRAY,
	      fn.getType(), EXPRmode, args, Type.AnyType);
	case _ =>
      }
      i = i + 1
    }
  }

  /** Attribute a template
  */
  def transformTemplate(templ: Tree$Template, owner: Symbol): Tree$Template = {
    if (global.debug) global.log("transforming template of " + owner);//debug
    if (templ.getType() == null)
      defineTemplate(templ, owner, owner.members());//may happen for mixins
    //System.out.println(owner.info());//DEBUG
    val parents = templ.parents;
    transformConstrInvocationArgs(parents);
    if (owner.kind != ERROR) {
      validateParentClasses(parents, owner.info().parents(), owner.typeOfThis());
    }
    pushContext(templ, owner, owner.members());
    /*
    val params: Scope = new Scope();
    def computeParams(t: Type): unit = t match {
      case Type$PolyType(tparams, t1) =>
	var i = 0; while (i < tparams.length) {
	  params.enter(tparams(i));
	  i = i + 1;
	}
	computeParams(t1);
      case Type$MethodType(vparams, _) =>
	var i = 0; while (i < vparams.length) {
	  params.enter(vparams(i));
	  i = i + 1;
	}
      case _ =>
    }
    computeParams(owner.primaryConstructor().getType());
    pushContext(templ, owner.primaryConstructor(), params);
    */
    templ.setSymbol(TermSymbol.newLocalDummy(owner));
    val body1 = transformStatSeq(templ.body, templ.symbol());
    /*
    popContext();
    */
    popContext();
    if (owner.isTrait()) {
      var i = 0; while (i < parents.length) {
	checkPureConstr(parents(i), owner);
	if (i >= 1) checkTrait(parents(i), owner);
	i = i + 1
      }
      var j = 0; while (j < templ.body.length) {
	checkPureDef(body1(j), owner);
	j = j + 1
      }
    }
    val templ1: Tree$Template = copy.Template(templ, parents, body1);
    templ1.setType(owner.getType());
    templ1
  }

  /** Attribute an argument list.
  *  @param pos      Position for error reporting
  *  @param meth     The symbol of the called method, or `null' if none exists.
  *  @param tparams  The type parameters that need to be instantiated
  *  @param methtype The method's type w/o type parameters
  *  @param argMode  The argument mode (either EXPRmode or PATTERNmode)
  *  @param args     The actual arguments
  *  @param pt       The proto-resulttype.
  *  @return         The vector of instantiated argument types, or null if error.
  */
  def transformArgs(pos: int, meth: Symbol, tparams: Array[Symbol], methtype: Type, argMode: int, args: Array[Tree], pt: Type): Array[Type] = {
    //System.out.println("trans args " + meth + ArrayApply.toString(tparams) + ":" + methtype + "," + pt);//DEBUG
    val argtypes = new Array[Type](args.length);
    methtype match {
      case Type$MethodType(params, restp) =>
	val formals = infer.formalTypes(params, args.length);
	if (formals.length != args.length) {
	  error(pos, "wrong number of arguments for " +
		(if (meth == null) "<function>" else meth) +
		ArrayApply.toString(formals.asInstanceOf[Array[Object]], "(", ",", ")"));
	  return null;
	}
	if (tparams.length == 0) {
	  var i = 0; while (i < args.length) {
	    args(i) = transform(args(i), argMode, formals(i));
	    argtypes(i) = args(i).getType().deconst();
	    i = i + 1
	  }
	} else {
	  // targs: the type arguments inferred from the prototype
	  val targs: Array[Type] = infer.protoTypeArgs(tparams, restp, pt, params);

	  // argpts: prototypes for arguments
	  val argpts = new Array[Type](formals.length);
	  var i = 0; while (i < formals.length) {
	    argpts(i) = formals(i).subst(tparams, targs);
	    i = i + 1
	  }

 	  // transform arguments with (targs/tparams)formals as prototypes
	  { var i = 0; while (i < args.length) {
	    args(i) = transform(
	      args(i), argMode | POLYmode, formals(i).subst(tparams, targs));
	    i = i + 1
	  }}

	  // targs1: same as targs except that every AnyType is mapped to
	  // formal parameter type.
	  val targs1 = new Array[Type](targs.length);
	  { var i = 0; while (i < targs.length) {
	    targs1(i) = if (targs(i) != Type.AnyType) targs(i)
			else tparams(i).getType();
	    i = i + 1
	  }}

	  { var i = 0; while (i < args.length) {
	    argtypes(i) = args(i).getType().deconst();
	    argtypes(i) match {
	      case Type$PolyType(tparams1, restype1) =>
		argtypes(i) = infer.argumentTypeInstance(
		  tparams1, restype1,
		  formals(i).subst(tparams, targs1),
		  argpts(i));
	      case _ =>
	    }
	    i = i + 1
	  }}
	}
	//   desugarizing ident patterns
	if (params.length > 0 && (params(params.length-1).flags & REPEATED) != 0) {
	  if ((mode & PATTERNmode) != 0) {
	    def desug_allIdentPatterns(trees: Array[Tree], currentOwner: Symbol): unit = {
	      var i = 0; while (i < trees.length) {
		trees(i) match {
		  case Tree$Ident(name) =>
		    if (name != Names.PATTERN_WILDCARD) {
		      val vble: Symbol = context.scope.lookup(name);
		      trees(i) = desugarize.IdentPattern(trees(i)).setSymbol(vble)
			.setType(vble.getType());
		    } else {
                      trees(i) = gen.Ident(trees(i).pos, definitions.PATTERN_WILDCARD);
                    }
		  case _ =>
		}
		i = i + 1
	      }
	    }
    	    desug_allIdentPatterns(args, context.owner);
	  } else {
	    assert(args.length != params.length ||
		   !(args(params.length-1).isInstanceOf[Tree$Sequence]));
	  }
	}
	argtypes;

      case Type$PolyType(tparams1, restp) =>
	var tparams2: Array[Symbol] = tparams1;
	if (tparams.length != 0) {
	  tparams2 = new Array[Symbol](tparams.length + tparams1.length);
	  System.arraycopy(tparams, 0, tparams2, 0, tparams.length);
	  System.arraycopy(tparams1, 0, tparams2, tparams.length, tparams1.length);
	}
	transformArgs(pos, meth, tparams2, restp, argMode, args, pt)

      case Type.ErrorType =>
	var i = 0; while (i < args.length) {
	  args(i) = transform(args(i), argMode, Type.ErrorType);
	  argtypes(i) = args(i).getType().deconst();
	  i = i + 1
	}
	argtypes

      case _ =>
	var i = 0; while (i < args.length) {
	  args(i) = transform(args(i), argMode, Type.AnyType);
	  argtypes(i) = args(i).getType().deconst();
	  i = i + 1
	}
	argtypes
    }
  }

  /** Atribute an expression or pattern with prototype `pt'.
  *  Check that expression's type conforms to `pt'.
  *  Resolve overloading and apply parameterless functions.
  *  Insert `apply' function if needed.
  */
  def transform(tree: Tree, mode: int, pt: Type): Tree = {
    val savedMode: int = this.mode;
    val savedPt: Type = this.pt;
    this.mode = mode;
    this.pt = pt;
    val tree1: Tree = adapt(transform(tree), mode, pt);

    assert(tree.getType() != Type.AnyType, tree);//debug

    //new TextTreePrinter().print(tree1).print(": " + tree1.getType()).println().end();//DEBUG

    this.mode = savedMode;
    this.pt = savedPt;
    tree1
  }

  /** Transform expression or type with a given mode.
  */
  def transform(tree: Tree, mode: int): Tree = {
    if ((mode & (EXPRmode | PATTERNmode | CONSTRmode)) != 0)
      return transform(tree, mode, Type.AnyType);

    val savedMode: int = this.mode;
    this.mode = mode;
    val tree1: Tree = transform(tree);
    this.mode = savedMode;

    if ((mode & TYPEmode) != 0) {
      val sym: Symbol = tree1.symbol();
      if ((mode & FUNmode) == 0 && sym != null && sym.typeParams().length != 0)
	return error(tree.pos, "" + sym + " takes type parameters.")
//	else if (tree1.isType())
//	    return gen.mkType(tree1.pos, tree1.getType());
    }
    tree1
  }

  def transform(trees: Array[Tree], mode: int): Array[Tree] = {
    var i = 0; while (i < trees.length) {
      trees(i) = transform(trees(i), mode);
      i = i + 1
    }
    trees
  }

  override def transform(trees: Array[Tree]): Array[Tree] =
    super.transform(trees);

  override def transform(trees: Array[Tree$CaseDef]): Array[Tree$CaseDef] =
    super.transform(trees);

  override def transform(trees: Array[Tree$AbsTypeDef]): Array[Tree$AbsTypeDef] =
    super.transform(trees);

  override def transform(trees: Array[Tree$ValDef]): Array[Tree$ValDef] =
    super.transform(trees);

  override def transform(trees: Array[Array[Tree$ValDef]]): Array[Array[Tree$ValDef]] =
    super.transform(trees);

  /** The main attribution function
  */
  override def transform(tree: Tree): Tree = {

    def qualifyingClass(tree: Tree, name: Name): Symbol = {
      if (name == TypeNames.EMPTY) {
        val clazz: Symbol = context.enclClass.owner;
        if (clazz != null)
	  clazz
	else {
          error(tree.pos, "" + tree +
		" can be used only in a class, object, or template");
	  Symbol.ERROR
	}
      } else {
	var i: Context = context;
	while (i != Context.NONE &&
	       !(i.owner.kind == CLASS && i.owner.name == name))
	  i = i.outer;
	if (i != Context.NONE)
	  i.owner
	else {
          error(tree.pos, "" + name + " is not an enclosing class");
	  Symbol.ERROR
	}
      }
    }

    //System.out.println("transforming " + tree);//DEBUG
    if (tree.getType() != null) {
      checkDefined.all = tree; checkDefined.traverse(tree);//debug
      return tree;
    }
    val sym: Symbol = tree.symbol();
    if (sym != null && !sym.isInitialized()) sym.initialize();
    if (global.debug && TreeInfo.isDefinition(tree)) global.log("transforming definition of " + sym);
    try {
      tree match {
	case Tree$Bad() =>
	  tree.setSymbol(Symbol.ERROR).setType(Type.ErrorType)

	case Tree.Empty =>
	  tree.setType(Type.NoType)

	case Tree$PackageDef(pkg, templ @ Tree$Template(parents, body)) =>
	  val pkgSym: Symbol = pkg.symbol();
	  if (pkgSym != null && pkgSym.isPackage()) {
	    pushContext(templ, pkgSym, pkgSym.members());
	    val body1: Array[Tree] = transform(body);
	    popContext();
	    val templ1: Tree$Template = copy.Template(templ, parents, body1);
	    templ1.setType(Type.NoType).setSymbol(Symbol.NONE);
	    copy.PackageDef(tree, pkg, templ1)
	      .setType(Type.NoType);
	  } else {
	    tree.setType(Type.ErrorType);
	  }

	case Tree$PackageDef(_, _) =>
	  tree.setType(Type.ErrorType)

	case Tree$ClassDef(_, _, tparams, vparams, tpe, templ) =>
	  pushContext(
	    tree, sym.primaryConstructor(), new Scope(context.scope));
	  reenterParams(tparams, vparams, sym.primaryConstructor().getType());
	  val tparams1 = transform(tparams);
	  val vparams1 = transform(vparams);
	  checkNoEscapeParams(vparams1);
	  val tpe1: Tree = transform(tpe, TYPEmode);
	  if ((sym.flags & CASE) != 0 && vparams.length > 0 && templ.getType() == null)
	    templ.body = desugarize.addCaseElements(templ.body, vparams(0));

	  val templ1: Tree$Template = transformTemplate(templ, sym);
	  checkNoEscape(tree.pos, sym.info());
	  popContext();
	  copy.ClassDef(tree, sym, tparams1, vparams1, tpe1, templ1)
  	    .setType(Type.NoType);

	case Tree$ModuleDef(_, _, tpe, templ) =>
	  sym.moduleClass().initialize();
	  val tpe1: Tree = transform(tpe, TYPEmode);
	  val templ1: Tree$Template = transformTemplate(templ, sym.moduleClass());
	  if (tpe1 != Tree.Empty && !templ1.getType().isSubType(tpe1.getType()))
	    error(tree.pos, "" + sym + " does not implement " + tpe1.getType());
	  copy.ModuleDef(tree, sym, tpe1, templ1)
	    .setType(Type.NoType);

	case Tree$DefDef(_, name, tparams, vparams, tpe, rhs) =>
	  pushContext(tree, sym, new Scope(context.scope));
	  reenterParams(tparams, vparams, sym.getType());
	  if (name == Names.CONSTRUCTOR)
	    context.enclClass.owner.flags = context.enclClass.owner.flags | INCONSTRUCTOR;
	  val tparams1 = transform(tparams);
	  val vparams1 = transform(vparams);
	  checkNoEscapeParams(vparams1);
	  val tpe1: Tree =
	    if (tpe == Tree.Empty) gen.mkType(tree.pos, sym.getType().resultType())
	    else transform(tpe, TYPEmode);
	  var rhs1: Tree =
	    if (rhs == Tree.Empty) rhs
	    else transform(
	      rhs,
	      if (name == Names.CONSTRUCTOR) CONSTRmode else EXPRmode,
	      tpe1.getType());
	  popContext();
	  context.enclClass.owner.flags = context.enclClass.owner.flags & ~INCONSTRUCTOR;
	  sym.flags = sym.flags | LOCKED;
	  checkNonCyclic(tree.pos, tpe1.getType());
	  sym.flags = sym.flags & ~LOCKED;
	  copy.DefDef(tree, sym, tparams1, vparams1, tpe1, rhs1)
	    .setType(Type.NoType);

	case Tree$ValDef(_, _, tpe, rhs) =>
	  val tpe1: Tree =
	    if (tpe == Tree.Empty) gen.mkType(tree.pos, sym.getType())
	    else transform(tpe, TYPEmode);
	  var rhs1: Tree = rhs;
	  if (rhs != Tree.Empty) {
	    pushContext(tree, sym, context.scope);
	    rhs1 = transform(rhs, EXPRmode, tpe1.getType());
	    popContext();
	  } else if ((sym.flags & (MUTABLE | DEFERRED)) == MUTABLE) {
	    rhs1 = gen.mkDefaultValue(tree.pos, sym.getType());
	  }
	  sym.flags = sym.flags | LOCKED;
	  checkNonCyclic(tree.pos, tpe1.getType());
	  sym.flags = sym.flags & ~LOCKED;
	  copy.ValDef(tree, sym, tpe1, rhs1)
	    .setType(Type.NoType);

	case Tree$AbsTypeDef(_, _, rhs, lobound) =>
	  val rhs1: Tree = transform(rhs, TYPEmode);
	  val lobound1: Tree = transform(lobound, TYPEmode);
	  checkNonCyclic(tree.pos, sym.getType());
	  copy.AbsTypeDef(tree, sym, rhs1, lobound1)
	    .setType(Type.NoType);

	case Tree$AliasTypeDef(_, _, tparams, rhs) =>
	  pushContext(tree, sym.primaryConstructor(), new Scope(context.scope));
	  reenterParams(tparams, sym.typeParams());
	  val tparams1 = transform(tparams);
	  val rhs1: Tree = transform(rhs, TYPEmode);
	  popContext();
	  checkNonCyclic(tree.pos, sym.getType());
	  copy.AliasTypeDef(tree, sym, tparams1, rhs1)
	    .setType(Type.NoType);

	case Tree$Import(expr, selectors) =>
	  context.imports = new ImportList(tree, context.scope, context.imports);
	  Tree.Empty

	case Tree$Block(stats) =>
	  pushContext(tree, context.owner, new Scope(context.scope));
	  val stats1 = desugarize.Statements(stats, true);
	  enterSyms(stats1);
	  context.imports = context.outer.imports;
	  val curmode: int = mode;
	  val owntype: Type =
	    if ((curmode & CONSTRmode) != 0) {
	      stats1(0) = transform(stats1(0), curmode, pt);
	      context.enclClass.owner.flags = context.enclClass.owner.flags & ~INCONSTRUCTOR;
	      var i = 1; while (i < stats1.length) {
		stats1(i) = transform(stats1(i), EXPRmode);
		i = i + 1
	      }
	      stats1(0).getType()
	    } else {
	      var i = 0; while (i < stats1.length - 1) {
		stats1(i) = transform(stats1(i), EXPRmode);
		i = i + 1
	      }
	      if (stats1.length > 0) {
		stats1(stats1.length - 1) =
		  transform(stats1(stats1.length - 1), curmode & ~FUNmode, pt);
		checkNoEscape(tree.pos, stats1(stats1.length - 1).getType().deconst())
	      } else {
		definitions.UNIT_TYPE()
	      }
	    }
	  popContext();
	  copy.Block(tree, stats1)
	    .setType(owntype);

        case Tree$Sequence(trees) =>
          var i = 0; while (i < trees.length) {
	    trees(i) = transform(trees(i),
				 this.mode | SEQUENCEmode,
				 pt);
	    i = i + 1
          }
          copy.Sequence(tree, trees).setType(pt);

	case Tree$Alternative(choices) =>
	  val save: boolean = this.inAlternative;
	  this.inAlternative = true;

	  val newts = new Array[Tree](choices.length);
	  var i = 0; while (i < choices.length) {
	    newts(i) = transform(choices(i), this.mode, pt);
	    i = i + 1
	  }

	  //val tpe: Type = Type.lub(Tree.typeOf(newts));

	  this.inAlternative = save;
	  copy.Alternative(tree, newts).setType(pt);

	case Tree$Bind(name, body) =>
          var vble: Symbol = null;
          if(name != Names.PATTERN_WILDCARD) {
            vble = new TermSymbol(tree.pos,
                                  name,
                                  context.owner,
                                  0x00000000).setType(pt);
            enterInScope(vble);
            //System.out.println("Bind("+name+",...) enters in scope:"+vble.fullNameString());

            patternVars.put(vble, new Boolean(this.inAlternative));
            //System.out.println("case Bind.. put symbol vble="+vble+" in scope and patternVars.");
          }
	  val body1: Tree = transform(body);
          if(name == Names.PATTERN_WILDCARD) body1
	  else {
            //assert body1.getType() != null;
            if(TreeInfo.isSequenceValued(body1)) {
              vble.setType(definitions.LIST_TYPE(pt));
            } else {
              vble.setType(body1.getType());
            }
            copy.Bind(tree, name, body1)
              .setSymbol(vble).setType(body1.getType());
	  }

	case Tree$Visitor(cases) =>
	  if (pt.symbol().isSubClass(definitions.PARTIALFUNCTION_CLASS)) {
	    val pft: Type = pt.baseType(definitions.PARTIALFUNCTION_CLASS);
	    val pftargs = pft.typeArgs();
	    if (pftargs.length == 2 && infer.isFullyDefined(pftargs(0))) {
	      val pattype: Type = pftargs(0);
	      var restype: Type = pftargs(1);
	      val isDefinedAtVisitor: Tree = transformVisitor(
		desugarize.isDefinedAtVisitor(tree),
		pattype, definitions.BOOLEAN_TYPE());
	      val applyVisitor: Tree = transformVisitor(tree, pattype, restype);
	      if (!infer.isFullyDefined(restype))
		restype = applyVisitor.getType().deconst();
	      if (definitions.PARTIALFUNCTION_CLASS.isExternal())
		// need to load tree for mixins
		new SourceCompleter(global, true).complete(
		  definitions.PARTIALFUNCTION_CLASS);
	      gen.mkPartialFunction(
		tree.pos, applyVisitor, isDefinedAtVisitor,
		pattype, restype, context.owner);
	    } else {
	      error(tree.pos, "expected pattern type of cases could not be determined");
	    }
	  } else {
	    transform(desugarize.Visitor(tree))
	  }

	case Tree$Assign(Tree$Apply(_, _), _) =>
	  transform(desugarize.Update(tree))

	case Tree$Assign(lhs, rhs) =>
	  val lhs1: Tree = transform(lhs, EXPRmode);
	  val varsym: Symbol = lhs1.symbol();
	  if (isSetterMethod(varsym)) {
	    // todo: change this to require setters in same template
	    transform(desugarize.Assign(tree.pos, lhs, rhs));
	  } else if (varsym != null && (varsym.flags & MUTABLE) != 0) {
	    val rhs1: Tree = transform(rhs, EXPRmode, lhs1.getType());
	    copy.Assign(tree, lhs1, rhs1)
	      .setType(definitions.UNIT_TYPE());
	  } else {
	    if (lhs1.getType() != Type.ErrorType)
	      error(tree.pos, "assignment to non-variable ");
	    errorTree(tree.pos);
	  }

	case Tree$If(cond, thenp, elsep) =>
	  val cond1: Tree = transform(cond, EXPRmode, definitions.BOOLEAN_TYPE());
	  var thenp1: Tree = _, elsep1: Tree = _;
	  if (elsep == Tree.Empty) {
	    thenp1 = transform(thenp, EXPRmode, definitions.UNIT_TYPE());
	    elsep1 = make.Block(tree.pos, Tree.EMPTY_ARRAY)
	      .setType(definitions.UNIT_TYPE());
	  } else {
	    thenp1 = transform(thenp, EXPRmode, pt);
	    elsep1 = transform(elsep, EXPRmode, pt);
	  }
	  copy.If(tree, cond1, thenp1, elsep1)
	    .setType(Type.lub(NewArray.Type(thenp1.getType(), elsep1.getType())));

	case Tree$Throw(expr) =>
	  val expr1: Tree = transform(
	    expr, EXPRmode, definitions.JAVA_THROWABLE_TYPE());
	  gen.Select(tree.pos, expr1, definitions.JAVA_THROWABLE_THROW);

	case Tree$Return(expr) =>
	  if (!context.owner.isInitialized()) {
	    error(tree.pos, "method with return needs result type");
	  } else {
	    val enclFun: Symbol = context.owner.enclMethod();
	    if (enclFun.kind == VAL && !enclFun.isConstructor()) {
	      val expr1: Tree = transform(
		expr, EXPRmode, enclFun.getType().resultType());
	      copy.Return(tree, expr1)
		.setSymbol(enclFun).setType(definitions.ALL_TYPE());
	    } else {
	      error(tree.pos, "return outside method definition");
	    }
	  }

	case Tree$New(templ) =>
	  templ match {
	    case Tree$Template(parents, body) =>
	      if (parents.length == 1 && body.length == 0) {
		val parent1: Tree = transform(parents(0), CONSTRmode, pt);
		val owntype: Type = parent1.getType();
		val templ1: Tree$Template =
		  copy.Template(templ, Symbol.NONE, NewArray.Tree(parent1), body)
		  .setType(owntype).asInstanceOf[Tree$Template];
		checkInstantiatable(tree.pos, owntype);
		copy.New(tree, templ1)
		  .setType(owntype.instanceType());
	      } else {
		pushContext(tree, context.owner, new Scope(context.scope));
		val cd: Tree = make.ClassDef(
		  templ.pos,
		  0,
		  Names.ANON_CLASS_NAME.toTypeName(),
		  Tree.AbsTypeDef_EMPTY_ARRAY,
		  NewArray.ValDefArray(Tree.ValDef_EMPTY_ARRAY),
		  Tree.Empty,
		  templ);
		//new TextTreePrinter().print(cd).println().end();//DEBUG
		enterSym(cd);
		val cd1 = transform(cd);
		val clazz: Symbol = cd1.symbol();
		if (clazz.kind != CLASS)
		  errorTree(tree.pos)
		else {
		  // compute template's type with new refinement scope.
		  val parentTypes = clazz.info().parents();
		  val refinement: Scope = new Scope();
		  val base: Type = Type.compoundType(parentTypes, Scope.EMPTY);
		  val it: Scope$SymbolIterator = clazz.members().iterator();
		  while (it.hasNext()) {
		    val sym1: Symbol = it.next();
		    val basesym1: Symbol = base.lookupNonPrivate(sym1.name);
		    if (basesym1.kind != NONE &&
			!base.symbol().thisType().memberType(basesym1)
			.isSameAs(sym1.getType()))
		      refinement.enter(sym1);
		  }
		  val owntype =
		    if (refinement.isEmpty() && parentTypes.length == 1)
		      parentTypes(0)
		    else
		      checkNoEscape(
			tree.pos,
			Type.compoundType(parentTypes, refinement, clazz));
		  val alloc: Tree =
		    gen.New(
		      gen.Apply(
			gen.mkRef(
			  tree.pos,
			  Type.localThisType,
			  clazz.primaryConstructor()),
			Tree.EMPTY_ARRAY))
		    .setType(owntype);
		  popContext();
		  make.Block(tree.pos, NewArray.Tree(cd1, alloc))
		    .setType(owntype);
		}
	      }
	  }

	case Tree$Typed(expr, tpe) =>
	  expr match {
	    case Tree$Ident(n)
	      if (n != Names.PATTERN_WILDCARD && (mode & PATTERNmode) != 0) =>
                transform(desugarize.TypedPattern(tree.asInstanceOf[Tree$Typed]),
                          mode,
                          pt);
	    case _ =>
	      var expr1: Tree = _;
	      var tpe1: Tree = _;
	      tpe match {
		case Tree$Ident(TypeNames.WILDCARD_STAR) =>
		  expr1 = transform(
		    expr, mode & baseModes, definitions.SEQ_TYPE(pt));
		  val elemtps =
		    expr1.getType().baseType(definitions.SEQ_CLASS).typeArgs();
		  val elemtp: Type = if (elemtps.length == 1) elemtps(0)
				     else Type.ErrorType;

		  tpe1 = tpe.setType(elemtp);
                case _ =>
                    tpe1 = transform(tpe, TYPEmode);
		    expr1 = transform(expr, mode & baseModes, tpe1.getType());
		}
		copy.Typed(tree, expr1, tpe1).setType(tpe1.getType())
	  }

	case Tree$Function(vparams, body) =>
	  pushContext(tree, context.owner, new Scope(context.scope));
	  var restype: Type = desugarize.preFunction(vparams, pt);
	  enterParams(vparams);
	  val vparams1 = transform(vparams);
	  val body1: Tree = transform(body, EXPRmode, restype);
	  if (!infer.isFullyDefined(restype))
	    restype = body1.getType().deconst();
	  popContext();
	  gen.mkFunction(tree.pos, vparams1, body1, restype, context.owner);

	case Tree$TypeApply(fn, args) =>

	  val fn1: Tree = transform(
	    fn, (mode & (EXPRmode | CONSTRmode)) | FUNmode, Type.AnyType);
	  val args1 = transform(args, TYPEmode);
	  val argtypes = Tree.typeOf(args1);

	  // propagate errors in arguments
	  var i = 0;
	  while (i < argtypes.length && argtypes(i) != Type.ErrorType)
	    i = i + 1;
	  if (i < argtypes.length)
	    tree.setType(Type.ErrorType);
	  else {

	    // resolve overloading
	    fn1.getType() match {
	      case Type$OverloadedType(alts, alttypes) =>
		try {
		  infer.polyAlternative(fn1, alts, alttypes, args.length);
		} catch {
		  case ex: Type$Error => reportTypeError(tree.pos, ex);
		}
	      case _ =>
	    }

	    // match against arguments
	    fn1.getType() match {
	      case Type$PolyType(tparams, restp) if (tparams.length == argtypes.length) =>
		  /* constant fold asInstanceOf calls.
		  fn1 match {
		    case Tree$Select(qual, name) =>
		      if (fn1.symbol() == definitions.ANY_AS &&
			  qual.getType().isInstanceOf[Type$ConstantType]) {
			val restp1: Type = constfold.foldAsInstanceOf(
			  tree.pos,
			  qual.getType().asInstanceOf[Type$ConstantType],
			  argtypes(0));
			restp1 match {
			  case ConstantType(_, Object value) =>
			    return make.Literal(tree.pos, value)
			      .setType(restp1);
			  case =>
			}
		      }
		    case _ =>
		  }
		  */
		constfold.tryToFold(
		  copy.TypeApply(tree, fn1, args1)
		  .setType(restp.subst(tparams, argtypes)));

	      case Type.ErrorType =>
		tree.setType(Type.ErrorType)

	      case _ =>
		error(tree.pos,
		      infer.toString(fn1.symbol(), fn1.getType()) +
		      " cannot be applied to " +
		      ArrayApply.toString(
			argtypes.asInstanceOf[Array[Object]], "(", ",", ")"));
	    }
	  }

	case Tree$Apply(fn, args) =>
	  mode = mode & ~SEQUENCEmode;
	  var fn1: Tree = _;
	  var argMode: int = _;
	  var selfcc: boolean = false;
	  //todo: Should we pass in both cases a methodtype with
	  // AnyType's for args as a prototype?
	  if ((mode & EXPRmode) != 0) {
	    fn1 = transform(fn, mode | FUNmode, Type.AnyType);
	    argMode = EXPRmode;
	  } else if ((mode & PATTERNmode) != 0) {
	    fn1 = transform(fn, mode | FUNmode, pt);
	    argMode = PATTERNmode;
	  } else {
	    assert((mode & CONSTRmode) != 0);
	    fn1 = transform(fn, mode | FUNmode, Type.AnyType);
	    argMode = EXPRmode;

	    // convert type to constructor
	    val tsym: Symbol = TreeInfo.methSymbol(fn1);
	    if (tsym.kind != ERROR) {
	      assert(tsym.isType(), tsym);
	      fn1.getType().withDefaultArgs().unalias() match {
		case Type$TypeRef(pre, c, argtypes) =>
		  if (c.kind != CLASS) {
		    error(tree.pos,
			  "" + tsym + " is not a class; cannot be instantiated");
		  } else if (!pre.isStable()) {
		    error(tree.pos, "" + pre + " is not a legal prefix for a constructor");
		  } else {
		    c.initialize();
		    val constr: Symbol = c.allConstructors();
		    val fn0: Tree = fn1;
		    fn1 = gen.mkRef(fn1.pos, pre, constr);
		    fn1 match {
		      case Tree$Select(fn1qual, _) =>
			fn1.setType(checkAccessible(
			  fn1.pos, constr, fn1.getType(), fn1qual, fn1qual.getType()));
		      case _ =>
			if (constr.owner().isPackage())
			  fn1.setType(checkAccessible(
			    fn1.pos, constr, fn1.getType(), Tree.Empty, constr.owner().getType()));
		    }
		    if (tsym == c) {
		      fn0 match {
			case Tree$AppliedType(_, targs) =>
			  fn1 = gen.TypeApply(fn1, targs);
			case _ =>
		      }
		    } else {
		      // it was an alias type
		      // todo: handle overloaded constructors
		      if (argtypes.length != 0)
			fn1 = gen.TypeApply(
			  fn1, gen.mkTypes(fn1.pos, argtypes));
		      if (tsym.typeParams().length != 0 &&
			  !(fn0.isInstanceOf[Tree$AppliedType]))
			fn1.setType(new Type$PolyType(
			  tsym.typeParams(), fn1.getType()));
		    }
		    //System.out.println(TreeInfo.methSymbol(fn1) + ":" + tp + " --> " + fn1.getType() + " of " + fn1);//DEBUG
		    selfcc = TreeInfo.isSelfConstrCall(fn0);
		  }

		case _ =>
		  error(tree.pos,
			"" + tsym + " is not a class; cannot be instantiated");
	      }
	    }
	  }

	  // if function is overloaded with one alternative
	  // whose arity matches argument length, preselect this alternative.
	  fn1.getType() match {
	    case Type$OverloadedType(alts, alttypes) =>
	      var matching1: int = -1;
	      var matching2: int = -1;
	      for (val i <- Iterator.range(0, alttypes.length)) {
		// can't replace with while because of backend crash???
		val alttp: Type = alttypes(i) match {
		  case Type$PolyType(_, restp) => restp;
		  case t => t
		}
		alttp match {
		  case Type$MethodType(params, _) =>
		    if (params.length == args.length ||
			params.length > 0 &&
			args.length > params.length - 1 &&
			(params(params.length-1).flags & REPEATED) != 0) {
		      matching2 = matching1;
		      matching1 = i;
		    }
		  case _ =>
		}
	      }
	      if (matching1 >= 0 && matching2 < 0)
		fn1.setSymbol(alts(matching1)).setType(alttypes(matching1));
	    case _ =>
	  }

	  def handleApply: Tree = {
	    // handle the case of application of match to a visitor specially
	    if (args.length == 1 && args(0).isInstanceOf[Tree$Visitor]) {
	      val pattp: Type = matchQualType(fn1);
	      if (pattp == Type.ErrorType) {
		return tree.setType(Type.ErrorType)
	      } else if (pattp != Type.NoType) {
		if (infer.isFullyDefined(pattp) &&
		    !(fn1.getType().isInstanceOf[Type$PolyType] &&
		      pattp.containsSome(fn1.getType().typeParams()))) {
		  val fn2: Tree = desugarize.postMatch(fn1, context.enclClass.owner);
		  val arg1: Tree = transformVisitor(args(0), pattp, pt);
		  return copy.Apply(tree, fn2, NewArray.Tree(arg1))
			.setType(arg1.getType());
		} else {
		  return error(tree.pos, "expected pattern type of cases could not be determined") : Tree;
		}
	      }
	    }

	    // return prematurely if function is a superclass constructor
	    // and no type arguments need to be inferred.
	    if ((mode & SUPERmode) != 0 && fn1.getType().isInstanceOf[Type$MethodType]) {
	      return copy.Apply(tree, fn1, args).setType(fn1.getType().resultType()) : Tree;
	    }

	    // type arguments with formals as prototypes if they exist.
	    fn1.setType(infer.freshInstance(fn1.getType()));
	    val argtypes = transformArgs(
	      tree.pos, fn1.symbol(), Symbol.EMPTY_ARRAY, fn1.getType(), argMode, args, pt);

	    if (argtypes == null)
	      return tree.setType(Type.ErrorType)
	    else {
	      var i: int = 0;
	      while (i < argtypes.length && argtypes(i) != Type.ErrorType)
		i = i + 1;
	      if (i < argtypes.length) return tree.setType(Type.ErrorType);
	    }

	    // resolve overloading1g
	    fn1.getType() match {
	      case Type$OverloadedType(alts, alttypes) =>
		try {
		  infer.methodAlternative(fn1, alts, alttypes, argtypes, pt);
		} catch {
		  case ex: Type$Error => reportTypeError(tree.pos, ex);
		}
	      case _ =>
	    }

	    // check that self constructors go backwards.
	    if (selfcc) {
	      val constr: Symbol = TreeInfo.methSymbol(fn1);
	      if (constr != null && constr.kind == VAL &&
		  !(constr.getType().isInstanceOf[Type$OverloadedType]) &&
		  constr.pos > tree.pos)
		error(tree.pos,
		      "illegal forward reference to self constructor");
	    }

	    fn1.getType() match {
	      case Type$PolyType(tparams, restp) =>
		// if method is polymorphic,
		// infer instance, and adapt arguments to instantiated formals
		try {
		  fn1 = infer.methodInstance(fn1, tparams, restp, argtypes, pt);
		  //System.out.println(fn1 + ":" + fn1.getType());//DEBUG
		} catch {
		  case ex: Type$Error => reportTypeError(tree.pos, ex);
		}
		fn1.getType() match {
		  case Type$MethodType(params, restp1) =>
		    val formals = infer.formalTypes(params, args.length);
		    var i = 0; while (i < args.length) {
		      args(i) = adapt(args(i), argMode, formals(i));
                      args(i) match {
                        case Tree$Typed( arg, Tree$Ident( TypeNames.WILDCARD_STAR ) ) =>
                          if( i != args.length - 1 ) {
                            error( arg.pos, "escape only allowed in last position");
                          } else if ( args.length > params.length ) {
                            error( arg.pos, "escaping cannot be mixed with values");
                          }
                        case _ => /* nop */
                      }
		      i = i + 1
		    }
		    return constfold.tryToFold(
		      copy.Apply(tree, fn1, args)
		      .setType(restp1));
		  case _ =>
		}

	      case Type$MethodType(params, restp) =>
		// if method is monomorphic,
		// check that it can be applied to arguments.
		if (infer.isApplicable(fn1.getType(), argtypes, Type.AnyType)) {
		  return constfold.tryToFold(
		    copy.Apply(tree, fn1, args)
		    .setType(restp));
		}

	      case Type.ErrorType =>
		return tree.setType(Type.ErrorType);

	      case _ =>
	    }
	    error(
	      tree.pos,
	      infer.applyErrorMsg(
		"", fn1, " cannot be applied to ", argtypes, pt));
	  }

	  handleApply

	case Tree$Super(qualifier, mixin) =>
	  val clazz: Symbol = qualifyingClass(tree, qualifier);
          tree.setSymbol(clazz);
          if (clazz == Symbol.ERROR) {
	    tree.setType(Type.ErrorType);
	  } else {
	    val parents = clazz.parents();
	    if (mixin == TypeNames.EMPTY) {
	      tree.setType(parents(0).instanceType());
	    } else {
	      var i = 0;
	      while (i < parents.length && parents(i).symbol().name != mixin)
	      i = i + 1;
	      if (i < parents.length)
		tree.setType(parents(i).instanceType());
	      else
		error(tree.pos,
		      "" + mixin + " does not name a mixin base class of " + clazz);
	    }
	  }

	case Tree$This(name) =>
	  val clazz: Symbol = qualifyingClass(tree, name);
          tree.setSymbol(clazz);
          if (clazz == Symbol.ERROR)
	    tree.setType(Type.ErrorType);
	  else
 	    tree.setType(
	      if (pt != null && pt.isStable() || (mode & QUALmode) != 0) clazz.thisType()
	      else clazz.typeOfThis());

	case Tree$Select(qual, name) =>
	  val qualmode: int = EXPRmode | POLYmode | QUALmode;
	  var qual1: Tree = transform(qual, qualmode);
	  if (name.isTypeName())
	    qual1 = checkStable(qual1);
	  transformSelect(
	    tree, adapt(qual1, qualmode, Type.AnyType), name);

	case Tree$Ident(name) =>
	  if (name == Names.CONSTRUCTOR) {
	    assert((mode & CONSTRmode) != 0, tree);
	    copy.Ident(tree, context.enclClass.owner)
            .setType(context.enclClass.owner.getType());
	  } else if (((mode & (PATTERNmode | FUNmode)) == PATTERNmode) && name.isVariable()) {
            var vble: Symbol = null, vble2: Symbol = null;

	    // if vble is bound with @, there is already a symbol
            if (name != Names.PATTERN_WILDCARD) {
              vble2 = context.scope.lookup(name);
	    }
	    var tree1 = tree;
            if (patternVars.containsKey(vble2))
              vble = vble2;
            else {
              vble =
		if (name == Names.PATTERN_WILDCARD)
                  definitions.PATTERN_WILDCARD
		else new TermSymbol(tree.pos,
                                    name,
                                    context.owner,
                                    0).setType(pt);
	      //if(((mode & SEQUENCEmode) != 0)&&(name != Names.PATTERN_WILDCARD)) {
	      if(name != Names.PATTERN_WILDCARD) {
		// x => x @ _ in sequence patterns
		tree1 = desugarize.IdentPattern(tree);
	      }
	    }
	    if (name != Names.PATTERN_WILDCARD) enterInScope(vble);
	    tree1.setSymbol(vble).setType(pt);
	  } else {
	    transformIdent(tree, name)
	  }

	case Tree$Literal(value) =>
	  tree.setType(Type.constantType(value))

	case Tree$LabelDef(name, params, body) =>
	  assert(params.length == 0);
	  pushContext(tree, context.owner, new Scope(context.scope));
	  val lsym: Symbol = new TermSymbol(tree.pos, name, context.owner, LABEL);
	  lsym.setInfo(
	    new Type$MethodType(Symbol.EMPTY_ARRAY, definitions.UNIT_TYPE()));
	  context.scope.enter(lsym);
	  val body1: Tree = transform(body, mode, pt);
	  popContext();
	  copy.LabelDef(tree, lsym, params, body1)
	    .setSymbol(lsym).setType(definitions.UNIT_TYPE());

	case Tree$TypeTerm() =>
	  tree

	case Tree$SingletonType(ref) =>
	  val ref1: Tree = transform(ref, EXPRmode | QUALmode, Type.AnyType);
	  copy.SingletonType(tree, ref1)
	    .setType(checkObjectType(tree.pos, ref1.getType().resultType()));

	case Tree$SelectFromType(qual, name) =>
	  val qual1: Tree = transform(qual, TYPEmode);
	  transformSelect(tree, qual1, name);

	case Tree$CompoundType(parents, refinements) =>
	  val parents1 = transform(parents, TYPEmode);
	  val ptypes = Tree.typeOf(parents);
	  val members: Scope = new Scope();
	  val self: Type = Type.compoundType(ptypes, members);
	  val clazz: Symbol = self.symbol();
	  pushContext(tree, clazz, members);
	  var i = 0; while (i < refinements.length) {
	    val m = enterSym(refinements(i));
	    m.flags = m.flags | OVERRIDE;
	    i = i + 1
	  }
	  val refinements1 = transformStatSeq(refinements, Symbol.NONE);
	  popContext();
	  copy.CompoundType(tree, parents1, refinements1)
	    .setType(self)

	case Tree$AppliedType(tpe, args) =>
	  val tpe1: Tree = transform(tpe, mode | FUNmode);
	  val args1 = transform(args, TYPEmode);
	  val argtypes = Tree.typeOf(args);
	  val tparams = tpe1.getType().symbol().typeParams();
	  var owntype: Type = Type.ErrorType;
	  if (tpe1.getType() != Type.ErrorType) {
	    if (tparams.length == args.length)
	      owntype = Type.appliedType(tpe1.getType(), argtypes);
	    else if (tparams.length == 0)
	      error(tree.pos, "" + tpe1.getType() + " does not take type parameters");
	    else
	      error(tree.pos, "wrong number of type arguments for " +
		    tpe1.getType());
	  }
	  copy.AppliedType(tree, tpe1, args1).setType(owntype);

	case Tree$FunType(_, _) =>
	  transform(desugarize.FunType(tree))

	case _ =>
	  throw new ApplicationError("illegal tree: " + tree)
      }
    } catch {
      case ex: Type$Error =>
	reportTypeError(tree.pos, ex);
	tree.setType(Type.ErrorType);
	if (tree.hasSymbol()) {
	  if (tree.symbol() != null) tree.symbol().setInfo(Type.ErrorType);
	  else tree.setSymbol(Symbol.ERROR);
	}
	tree;
    }
  }
}
}

//  LocalWords:  SOcos
