/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package typechecker

import symtab.Flags
import symtab.Flags._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Synthetic method implementations for case classes and case objects.
 *
 *  Added to all case classes/objects:
 *    def productArity: Int
 *    def productElement(n: Int): Any
 *    def productPrefix: String
 *    def productIterator: Iterator[Any]
 *
 *  Selectively added to case classes/objects, unless a non-default
 *  implementation already exists:
 *    def equals(other: Any): Boolean
 *    def hashCode(): Int
 *    def canEqual(other: Any): Boolean
 *    def toString(): String
 *
 *  Special handling:
 *    protected def readResolve(): AnyRef
 */
trait SyntheticMethods extends ast.TreeDSL {
  self: Analyzer =>

  import global._
  import definitions._
  import CODE._

  private object util {
    private type CM[T] = ClassManifest[T]

    lazy val IteratorModule = getModule("scala.collection.Iterator")
    lazy val Iterator_apply = getMember(IteratorModule, nme.apply)
    def iteratorOfType(tp: Type) = appliedType(IteratorClass.typeConstructor, List(tp))

    def ValOrDefDef(sym: Symbol, body: Tree) =
      if (sym.isLazy) ValDef(sym, body)
      else DefDef(sym, body)

    /** To avoid unchecked warnings on polymorphic classes.
     */
    def clazzTypeToTest(clazz: Symbol) = clazz.tpe.normalize match {
      case TypeRef(_, sym, args) if args.nonEmpty => ExistentialType(sym.typeParams, clazz.tpe)
      case tp                                     => tp
    }

    def makeMethodPublic(method: Symbol): Symbol = {
      method.privateWithin = NoSymbol
      method resetFlag AccessFlags
    }

    def methodArg(method: Symbol, idx: Int): Tree = Ident(method.paramss.head(idx))

    private def applyTypeInternal(manifests: List[CM[_]]): Type = {
      val symbols = manifests map manifestToSymbol
      val container :: args = symbols
      val tparams = container.typeConstructor.typeParams

      // Overly conservative at present - if manifests were more usable
      // this could do a lot more.
      require(symbols forall (_ ne NoSymbol), "Must find all manifests: " + symbols)
      require(container.owner.isPackageClass, "Container must be a top-level class in a package: " + container)
      require(tparams.size == args.size, "Arguments must match type constructor arity: " + tparams + ", " + args)
      require(args forall (_.typeConstructor.typeParams.isEmpty), "Arguments must be unparameterized: " + args)

      typeRef(container.typeConstructor.prefix, container, args map (_.tpe))
    }

    def manifestToSymbol(m: CM[_]): Symbol = m match {
      case x: scala.reflect.AnyValManifest[_] => definitions.getClass("scala." + x)
      case _                                  => getClassIfDefined(m.erasure.getName)
    }
    def companionType[T](implicit m: CM[T]) =
      getModule(m.erasure.getName).tpe

    // Use these like `applyType[List, Int]` or `applyType[Map, Int, String]`
    def applyType[M](implicit m1: CM[M]): Type =
      applyTypeInternal(List(m1))

    def applyType[M[X1], X1](implicit m1: CM[M[_]], m2: CM[X1]): Type =
      applyTypeInternal(List(m1, m2))

    def applyType[M[X1, X2], X1, X2](implicit m1: CM[M[_,_]], m2: CM[X1], m3: CM[X2]): Type =
      applyTypeInternal(List(m1, m2, m3))

    def applyType[M[X1, X2, X3], X1, X2, X3](implicit m1: CM[M[_,_,_]], m2: CM[X1], m3: CM[X2], m4: CM[X3]): Type =
      applyTypeInternal(List(m1, m2, m3, m4))
  }
  import util._

  class MethodSynthesis(val clazz: Symbol, localTyper: Typer) {
    private def isOverride(method: Symbol) =
      clazzMember(method.name).alternatives exists (sym => (sym != method) && !sym.isDeferred)

    private def setMethodFlags(method: Symbol): Symbol = {
      val overrideFlag = if (isOverride(method)) OVERRIDE else 0L

      method setFlag (overrideFlag | SYNTHETIC) resetFlag DEFERRED
    }

    private def finishMethod(method: Symbol, f: Symbol => Tree): Tree = {
      setMethodFlags(method)
      clazz.info.decls enter method
      logResult("finishMethod")(localTyper typed ValOrDefDef(method, f(method)))
    }

    private def createInternal(name: Name, f: Symbol => Tree, info: Type): Tree = {
      val m = clazz.newMethod(clazz.pos.focus, name.toTermName)
      m setInfo info
      finishMethod(m, f)
    }
    private def createInternal(name: Name, f: Symbol => Tree, infoFn: Symbol => Type): Tree = {
      val m = clazz.newMethod(clazz.pos.focus, name.toTermName)
      m setInfo infoFn(m)
      finishMethod(m, f)
    }

    private def cloneInternal(original: Symbol, f: Symbol => Tree, name: Name): Tree = {
      val m = original.cloneSymbol(clazz) setPos clazz.pos.focus
      m.name = name
      finishMethod(m, f)
    }

    private def cloneInternal(original: Symbol, f: Symbol => Tree): Tree =
      cloneInternal(original, f, original.name)

    def clazzMember(name: Name) = clazz.info nonPrivateMember name match {
      case NoSymbol => log("In " + clazz + ", " + name + " not found: " + clazz.info) ; NoSymbol
      case sym      => sym
    }
    def typeInClazz(sym: Symbol) = clazz.thisType memberType sym

    /** Function argument takes the newly created method symbol of
     *  the same type as `name` in clazz, and returns the tree to be
     *  added to the template.
     */
    def overrideMethod(name: Name)(f: Symbol => Tree): Tree =
      overrideMethod(clazzMember(name))(f)

    def overrideMethod(original: Symbol)(f: Symbol => Tree): Tree =
      cloneInternal(original, sym => f(sym setFlag OVERRIDE))

    def deriveMethod(original: Symbol, nameFn: Name => Name)(f: Symbol => Tree): Tree =
      cloneInternal(original, f, nameFn(original.name))

    def createMethod(name: Name, paramTypes: List[Type], returnType: Type)(f: Symbol => Tree): Tree =
      createInternal(name, f, (m: Symbol) => MethodType(m newSyntheticValueParams paramTypes, returnType))

    def createMethod(name: Name, returnType: Type)(f: Symbol => Tree): Tree =
      createInternal(name, f, NullaryMethodType(returnType))

    def createMethod(original: Symbol)(f: Symbol => Tree): Tree =
      createInternal(original.name, f, original.info)

    def forwardMethod(original: Symbol, newMethod: Symbol)(transformArgs: List[Tree] => List[Tree]): Tree =
      createMethod(original)(m => gen.mkMethodCall(newMethod, transformArgs(m.paramss.head map Ident)))

    def createSwitchMethod(name: Name, range: Seq[Int], returnType: Type)(f: Int => Tree) = {
      createMethod(name, List(IntClass.tpe), returnType) { m =>
        val arg0    = methodArg(m, 0)
        val default = DEFAULT ==> THROW(IndexOutOfBoundsExceptionClass, arg0)
        val cases   = range.map(num => CASE(LIT(num)) ==> f(num)).toList :+ default

        Match(arg0, cases)
      }
    }

    // def foo() = constant
    def constantMethod(name: Name, value: Any): Tree = {
      val constant = Constant(value)
      createMethod(name, Nil, constant.tpe)(_ => Literal(constant))
    }
    // def foo = constant
    def constantNullary(name: Name, value: Any): Tree = {
      val constant = Constant(value)
      createMethod(name, constant.tpe)(_ => Literal(constant))
    }
  }

  /** Add the synthetic methods to case classes.
   */
  def addSyntheticMethods(templ: Template, clazz0: Symbol, context: Context): Template = {
    if (phase.erasedTypes)
      return templ

    val synthesizer = new MethodSynthesis(
      clazz0,
      newTyper( if (reporter.hasErrors) context makeSilent false else context )
    )
    import synthesizer._

    def accessors = clazz.caseFieldAccessors
    def arity     = accessors.size
    // If this is ProductN[T1, T2, ...], accessorLub is the lub of T1, T2, ..., .
    // !!! Hidden behind -Xexperimental due to bummer type inference bugs.
    // Refining from Iterator[Any] leads to types like
    //
    //    Option[Int] { def productIterator: Iterator[String] }
    //
    // appearing legitimately, but this breaks invariant places
    // like Manifests and Arrays which are not robust and infer things
    // which they shouldn't.
    val accessorLub  = (
      if (opt.experimental)
        global.weakLub(accessors map (_.tpe.finalResultType))._1 match {
          case RefinedType(parents, decls) if !decls.isEmpty => intersectionType(parents)
          case tp                                            => tp
        }
      else AnyClass.tpe
    )

    def forwardToRuntime(method: Symbol): Tree =
      forwardMethod(method, getMember(ScalaRunTimeModule, "_" + method.name toTermName))(This(clazz) :: _)

    // Any member, including private
    def hasConcreteImpl(name: Name) =
      clazz.info.member(name).alternatives exists (m => !m.isDeferred && !m.isSynthetic)

    def hasOverridingImplementation(meth: Symbol) = {
      val sym = clazz.info nonPrivateMember meth.name
      sym.alternatives filterNot (_ eq meth) exists { m0 =>
        !m0.isDeferred && !m0.isSynthetic && (typeInClazz(m0) matches typeInClazz(meth))
      }
    }
    def readConstantValue[T](name: String, default: T = null.asInstanceOf[T]): T = {
      clazzMember(name.toTermName).info match {
        case NullaryMethodType(ConstantType(Constant(value))) => value.asInstanceOf[T]
        case _                                                => default
      }
    }
    def productIteratorMethod = {
      createMethod(nme.productIterator, iteratorOfType(accessorLub))(_ =>
        gen.mkMethodCall(ScalaRunTimeModule, "typedProductIterator", List(accessorLub), List(This(clazz)))
      )
    }
    def projectionMethod(accessor: Symbol, num: Int) = {
      createMethod(nme.productAccessorName(num), accessor.tpe.resultType)(_ => REF(accessor))
    }

    /** Common code for productElement and (currently disabled) productElementName
     */
    def perElementMethod(name: Name, returnType: Type)(caseFn: Symbol => Tree): Tree =
      createSwitchMethod(name, accessors.indices, returnType)(idx => caseFn(accessors(idx)))

    // def productElementNameMethod = perElementMethod(nme.productElementName, StringClass.tpe)(x => LIT(x.name.toString))

    /** The canEqual method for case classes.
     *    def canEqual(that: Any) = that.isInstanceOf[This]
     */
    def canEqualMethod: Tree = {
      createMethod(nme.canEqual_, List(AnyClass.tpe), BooleanClass.tpe)(m =>
        methodArg(m, 0) IS_OBJ clazzTypeToTest(clazz)
      )
    }

    /** The equality method for case classes.
     *  0 args:
     *    def equals(that: Any) = that.isInstanceOf[this.C] && that.asInstanceOf[this.C].canEqual(this)
     *  1+ args:
     *    def equals(that: Any) = (this eq that.asInstanceOf[AnyRef]) || {
     *      (that.isInstanceOf[this.C]) && {
     *        val x$1 = that.asInstanceOf[this.C]
     *        (this.arg_1 == x$1.arg_1) && (this.arg_2 == x$1.arg_2) && ... && (x$1 canEqual this)
     *       }
     *    }
     */
    def equalsClassMethod: Tree = createMethod(nme.equals_, List(AnyClass.tpe), BooleanClass.tpe) { m =>
      val arg0      = methodArg(m, 0)
      val thatTest  = gen.mkIsInstanceOf(arg0, clazzTypeToTest(clazz), true, false)
      val thatCast  = arg0 AS_ATTR clazz.tpe

      def argsBody: Tree = {
        val otherName = context.unit.freshTermName(clazz.name + "$")
        val otherSym  = m.newValue(m.pos, otherName) setInfo clazz.tpe setFlag SYNTHETIC
        val pairwise  = accessors map (acc => fn(Select(This(clazz), acc), acc.tpe member nme.EQ, Select(Ident(otherSym), acc)))
        val canEq     = gen.mkMethodCall(otherSym, nme.canEqual_, Nil, List(This(clazz)))
        def block     = Block(ValDef(otherSym, thatCast), AND(pairwise :+ canEq: _*))

        (This(clazz) ANY_EQ arg0) OR {
          thatTest AND Block(
            ValDef(otherSym, thatCast),
            AND(pairwise :+ canEq: _*)
          )
        }
      }
      if (accessors.isEmpty)
        thatTest AND ((thatCast DOT nme.canEqual_)(This(clazz)))
      else
        argsBody
    }

    def newAccessorMethod(ddef: DefDef): Tree = {
      deriveMethod(ddef.symbol, name => context.unit.freshTermName(name + "$")) { newAcc =>
        makeMethodPublic(newAcc)
        newAcc resetFlag (ACCESSOR | PARAMACCESSOR)
        ddef.rhs.duplicate
      }
    }

    // A buffer collecting additional methods for the template body
    val ts = new ListBuffer[Tree]

    def makeAccessorsPublic() {
      // If this case class has fields with less than public visibility, their getter at this
      // point also has those permissions.  In that case we create a new, public accessor method
      // with a new name and remove the CASEACCESSOR flag from the existing getter.  This complicates
      // the retrieval of the case field accessors (see def caseFieldAccessors in Symbols.)
      def needsService(s: Symbol) = s.isMethod && s.isCaseAccessor && !s.isPublic
      for (ddef @ DefDef(_, _, _, _, _, _) <- templ.body; if needsService(ddef.symbol)) {
        ts += newAccessorMethod(ddef)
        ddef.symbol resetFlag CASEACCESSOR
      }
    }
    /** The _1, _2, etc. methods to implement ProductN.
     */
    def productNMethods = {
      val accs = accessors.toIndexedSeq
      1 to arity map (num => productProj(arity, num) -> (() => projectionMethod(accs(num - 1), num)))
    }

    // methods for both classes and objects
    def productMethods = {
      List(
        Product_productPrefix   -> (() => constantNullary(nme.productPrefix, clazz.name.decode)),
        Product_productArity    -> (() => constantNullary(nme.productArity, arity)),
        Product_productElement  -> (() => perElementMethod(nme.productElement, accessorLub)(Ident)),
        Product_iterator        -> (() => productIteratorMethod),
        Product_canEqual        -> (() => canEqualMethod)
        // This is disabled pending a reimplementation which doesn't add any
        // weight to case classes (i.e. inspects the bytecode.)
        // Product_productElementName  -> (() => productElementNameMethod(accessors)),
      )
    }

    def caseClassMethods = productMethods ++ productNMethods ++ Seq(
      Object_hashCode -> (() => forwardToRuntime(Object_hashCode)),
      Object_toString -> (() => forwardToRuntime(Object_toString)),
      Object_equals   -> (() => equalsClassMethod)
    )

    def caseObjectMethods = productMethods ++ Seq(
      Object_hashCode -> (() => constantMethod(nme.hashCode_, clazz.name.decode.hashCode)),
      Object_toString -> (() => constantMethod(nme.toString_, clazz.name.decode))
      // Not needed, as reference equality is the default.
      // Object_equals   -> (() => createMethod(Object_equals)(m => This(clazz) ANY_EQ methodArg(m, 0)))
    )

    def addReadResolve() {
      /** If you serialize a singleton and then deserialize it twice,
       *  you will have two instances of your singleton, unless you implement
       *  the readResolve() method (see http://www.javaworld.com/javaworld/
       *  jw-04-2003/jw-0425-designpatterns_p.html)
       */
      if (clazz.isSerializable && !hasConcreteImpl(nme.readResolve)) {
        // Aha, I finally decoded the original comment.
        // This method should be generated as private, but apparently if it is, then
        // it is name mangled afterward.  (Wonder why that is.) So it's only protected.
        // For sure special methods like "readResolve" should not be mangled.
        ts += createMethod(nme.readResolve, Nil, ObjectClass.tpe) { m =>
          m setFlag PRIVATE
          REF(clazz.sourceModule)
        }
      }
    }

    def synthesize() = {
      if (clazz.isCase) {
        makeAccessorsPublic()
        val methods = if (clazz.isModuleClass) caseObjectMethods else caseClassMethods

        for ((m, impl) <- methods ; if !hasOverridingImplementation(m))
          ts += impl()
      }
      // Only nested objects inside objects should get readResolve automatically.
      // Otherwise, after de-serialization we get null references for lazy accessors
      // (nested object -> lazy val + class def) since the bitmap gets serialized but
      // the moduleVar not.
      if (clazz.isModuleClass && clazz.owner.isModuleClass)
        addReadResolve()
    }

    try synthesize()
    catch { case _: TypeError if reporter.hasErrors => () }

    if (phase.id <= currentRun.typerPhase.id) {
      treeCopy.Template(templ, templ.parents, templ.self,
        if (ts.isEmpty) templ.body else templ.body ++ ts // avoid copying templ.body if empty
      )
    }
    else templ
  }
}
