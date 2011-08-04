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

  import global._                  // the global environment
  import definitions._             // standard classes and methods

  /** Add the synthetic methods to case classes.
   */
  def addSyntheticMethods(templ: Template, clazz: Symbol, context: Context): Template = {
    import CODE._

    val localTyper = newTyper(
      if (reporter.hasErrors) context makeSilent false else context
    )
    def accessorTypes = clazz.caseFieldAccessors map (_.tpe.finalResultType)
    def accessorLub   = global.weakLub(accessorTypes)._1

    def hasOverridingImplementation(meth: Symbol): Boolean = {
      val sym = clazz.info nonPrivateMember meth.name
      def isOverride(s: Symbol) = {
        s != meth && !s.isDeferred && !s.isSynthetic &&
        (clazz.thisType.memberType(s) matches clazz.thisType.memberType(meth))
      }
      sym.alternatives exists isOverride
    }

    def syntheticMethod(name: Name, flags: Int, tpeCons: Symbol => Type) =
      newSyntheticMethod(name, flags | OVERRIDE, tpeCons)

    def newSyntheticMethod(name: Name, flags: Int, tpeCons: Symbol => Type) = {
      val method = clazz.newMethod(clazz.pos.focus, name.toTermName) setFlag flags

      clazz.info.decls enter (method setInfo tpeCons(method))
    }

    def newTypedMethod(method: Symbol)(body: Tree): Tree =
      localTyper typed { DEF(method) === body }

    def makeNoArgConstructor(res: Type) =
      (sym: Symbol) => MethodType(Nil, res)
    def makeTypeConstructor(args: List[Type], res: Type) =
      (sym: Symbol) => MethodType(sym newSyntheticValueParams args, res)
    def makeEqualityMethod(name: Name) =
      syntheticMethod(name, 0, makeTypeConstructor(List(AnyClass.tpe), BooleanClass.tpe))

    def newNullaryMethod(name: TermName, tpe: Type, body: Tree) = {
      val flags  = if (clazz.tpe.member(name) != NoSymbol) OVERRIDE else 0
      val method = clazz.newMethod(clazz.pos.focus, name) setFlag flags

      clazz.info.decls enter (method setInfo NullaryMethodType(tpe))
      newTypedMethod(method)(body)
    }
    def productPrefixMethod            = newNullaryMethod(nme.productPrefix, StringClass.tpe, LIT(clazz.name.decode))
    def productArityMethod(arity: Int) = newNullaryMethod(nme.productArity, IntClass.tpe, LIT(arity))
    def productIteratorMethod = {
      val method       = getMember(ScalaRunTimeModule, "typedProductIterator")
      val iteratorType = typeRef(NoPrefix, IteratorClass, List(accessorLub))

      newNullaryMethod(
        nme.productIterator,
        iteratorType,
        gen.mkMethodCall(method, List(accessorLub), List(This(clazz)))
      )
    }

    def projectionMethod(accessor: Symbol, num: Int) = {
      newNullaryMethod(nme.productAccessorName(num), accessor.tpe.resultType, REF(accessor))
    }

    /** Common code for productElement and (currently disabled) productElementName
     */
    def perElementMethod(accs: List[Symbol], methodName: Name, resType: Type, caseFn: Symbol => Tree): Tree = {
      val symToTpe  = makeTypeConstructor(List(IntClass.tpe), resType)
      val method    = syntheticMethod(methodName, 0, symToTpe)
      val arg       = method ARG 0
      val default   = List(DEFAULT ==> THROW(IndexOutOfBoundsExceptionClass, arg))
      val cases     =
        for ((sym, i) <- accs.zipWithIndex) yield
          CASE(LIT(i)) ==> caseFn(sym)

      newTypedMethod(method)(arg MATCH { cases ::: default : _* })
    }
    def productElementMethod(accs: List[Symbol]): Tree =
      perElementMethod(accs, nme.productElement, AnyClass.tpe, x => Ident(x))

    // def productElementNameMethod(accs: List[Symbol]): Tree =
    //   perElementMethod(accs, nme.productElementName, StringClass.tpe, x => Literal(x.name.toString))

    def moduleToStringMethod: Tree = {
      val method = syntheticMethod(nme.toString_, FINAL, makeNoArgConstructor(StringClass.tpe))
      newTypedMethod(method)(LIT(clazz.name.decode))
    }
    def moduleHashCodeMethod: Tree = {
      val method = syntheticMethod(nme.hashCode_, FINAL, makeNoArgConstructor(IntClass.tpe))
      // The string being used as hashcode basis is also productPrefix.
      newTypedMethod(method)(LIT(clazz.name.decode.hashCode))
    }

    def forwardingMethod(name: Name, targetName: Name): Tree = {
      val target      = getMember(ScalaRunTimeModule, targetName)
      val paramtypes  = target.tpe.paramTypes drop 1
      val method      = syntheticMethod(name, 0, makeTypeConstructor(paramtypes, target.tpe.resultType))

      newTypedMethod(method)(Apply(REF(target), This(clazz) :: (method ARGNAMES)))
    }

    /** The equality method for case modules:
     *    def equals(that: Any) = this eq that
     */
    def equalsModuleMethod: Tree = {
      val method = makeEqualityMethod(nme.equals_)

      newTypedMethod(method)(This(clazz) ANY_EQ (method ARG 0))
    }

    /** The canEqual method for case classes.
     *    def canEqual(that: Any) = that.isInstanceOf[This]
     */
    def canEqualMethod: Tree = {
      val method  = makeEqualityMethod(nme.canEqual_)

      newTypedMethod(method)((method ARG 0) IS_OBJ clazz.tpe)
    }

    /** The equality method for case classes.  The argument is an Any,
     *  but because of boxing it will always be an Object, so a check
     *  is neither necessary nor useful before the cast.
     *
     *   def equals(that: Any) =
     *     (this eq that.asInstanceOf[AnyRef]) ||
     *     (that match {
     *       case x @ this.C(this.arg_1, ..., this.arg_n) => x canEqual this
     *       case _                                       => false
     *     })
     */
    def equalsClassMethod: Tree = {
      val method           = makeEqualityMethod(nme.equals_)
      val that             = method ARG 0
      val constrParamTypes = clazz.primaryConstructor.tpe.paramTypes

      // returns (Apply, Bind)
      def makeTrees(acc: Symbol, cpt: Type): (Tree, Bind) = {
        val varName     = context.unit.freshTermName(acc.name + "$")
        val isRepeated  = isRepeatedParamType(cpt)
        val binding     = Bind(
          varName,
          if (isRepeated) Star(WILD.empty) else WILD.empty
        )
        val eqMethod: Tree  =
          if (isRepeated) gen.mkRuntimeCall(nme.sameElements, List(Ident(varName), Ident(acc)))
          else (Ident(varName) DOT nme.EQ)(Ident(acc))

        (eqMethod, binding)
      }

      // Creates list of parameters and a guard for each
      val (guards, params) = (clazz.caseFieldAccessors, constrParamTypes).zipped map makeTrees unzip

      // Verify with canEqual method before returning true.
      def canEqualCheck() = {
        val that: Tree              = (method ARG 0) AS clazz.tpe
        val canEqualOther: Symbol   = clazz.info nonPrivateMember nme.canEqual_

        typer typed { (that DOT canEqualOther)(This(clazz)) }
      }

      // Pattern is classname applied to parameters, and guards are all logical and-ed
      val guard = AND(guards: _*)
      // You might think this next line could be written
      //    val pat = Ident(clazz.companionModule) APPLY params
      // However this prevents the compiler from bootstrapping,
      // possibly due to a bug in how case classes are handled when
      // read from classfiles as opposed to from source.
      val pat   = Ident(clazz.name.toTermName) APPLY params

      newTypedMethod(method) {
        (This(clazz) ANY_EQ that) OR (that MATCH(
          (CASE(pat) IF guard)  ==> canEqualCheck()        ,
          DEFAULT               ==> FALSE
        ))
      }
    }

    def newAccessorMethod(tree: Tree): Tree = tree match {
      case DefDef(_, _, _, _, _, rhs) =>
        val newAcc = tree.symbol.cloneSymbol
        newAcc.name = context.unit.freshTermName(tree.symbol.name + "$")
        newAcc setFlag SYNTHETIC resetFlag (ACCESSOR | PARAMACCESSOR | PRIVATE | PROTECTED)
        newAcc.privateWithin = NoSymbol
        newAcc.owner.info.decls enter newAcc
        logResult("new accessor method")(newTypedMethod(newAcc)(rhs.duplicate))
    }

    def needsReadResolve = (
      // Only nested objects inside objects should get readResolve automatically.
      // Otherwise, after de-serialization we get null references for lazy accessors
      // (nested object -> lazy val + class def) since the bitmap gets serialized but
      // the moduleVar not.
      clazz.isSerializable && (clazz.owner.isPackageClass || clazz.owner.isModuleClass)
    )

    // A buffer collecting additional methods for the template body
    val ts = new ListBuffer[Tree]

    if (!phase.erasedTypes) try {
      if (clazz.isCase) {
        val isTop     = clazz.ancestors forall (x => !x.isCase)
        val accessors = clazz.caseFieldAccessors
        val arity     = accessors.size

        if (isTop) {
          // If this case class has fields with less than public visibility, their getter at this
          // point also has those permissions.  In that case we create a new, public accessor method
          // with a new name and remove the CASEACCESSOR flag from the existing getter.  This complicates
          // the retrieval of the case field accessors (see def caseFieldAccessors in Symbols.)
          def needsService(s: Symbol) = s.isMethod && s.isCaseAccessor && !s.isPublic
          for (stat <- templ.body ; if stat.isDef && needsService(stat.symbol)) {
            ts += newAccessorMethod(stat)
            stat.symbol resetFlag CASEACCESSOR
          }
        }
        /** The _1, _2, etc. methods to implement ProductN, and an override
         *  of productIterator with a more specific element type.
         *  Only enabled under -Xexperimental.
         */
        def productNMethods = {
          val projectionMethods = (accessors, 1 to arity).zipped map ((accessor, num) =>
            productProj(arity, num) -> (() => projectionMethod(accessor, num))
          )
          projectionMethods :+ (
            Product_iterator -> (() => productIteratorMethod)
          )
        }

        // methods for case classes only
        def classMethods = List(
          Object_hashCode -> (() => forwardingMethod(nme.hashCode_, "_" + nme.hashCode_)),
          Object_toString -> (() => forwardingMethod(nme.toString_, "_" + nme.toString_)),
          Object_equals   -> (() => equalsClassMethod)
        ) ++ (
          if (settings.Xexperimental.value) productNMethods
          else Nil
        )

        // methods for case objects only
        def objectMethods = List(
          Object_hashCode -> (() => moduleHashCodeMethod),
          Object_toString -> (() => moduleToStringMethod)
        )
        // methods for both classes and objects
        def everywhereMethods = {
          List(
            Product_productPrefix   -> (() => productPrefixMethod),
            Product_productArity    -> (() => productArityMethod(accessors.length)),
            Product_productElement  -> (() => productElementMethod(accessors)),
            // This is disabled pending a reimplementation which doesn't add any
            // weight to case classes (i.e. inspects the bytecode.)
            // Product_productElementName  -> (() => productElementNameMethod(accessors)),
            Product_canEqual        -> (() => canEqualMethod)
          )
        }

        val methods = (if (clazz.isModuleClass) objectMethods else classMethods) ++ everywhereMethods
        for ((m, impl) <- methods ; if !hasOverridingImplementation(m))
          ts += impl()
      }

      if (clazz.isModuleClass) {
        def hasReadResolve = {
          val sym = clazz.info member nme.readResolve // any member, including private
          sym.isTerm && !sym.isDeferred
        }

        /** If you serialize a singleton and then deserialize it twice,
         *  you will have two instances of your singleton, unless you implement
         *  the readResolve() method (see http://www.javaworld.com/javaworld/
         *  jw-04-2003/jw-0425-designpatterns_p.html)
         */
        if (!hasReadResolve && needsReadResolve) {
          // PP: To this day I really can't figure out what this next comment is getting at:
          // the !!! normally means there is something broken, but if so, what is it?
          //
          // !!! the synthetic method "readResolve" should be private, but then it is renamed !!!
          val method = newSyntheticMethod(nme.readResolve, PROTECTED, makeNoArgConstructor(ObjectClass.tpe))
          ts += newTypedMethod(method)(REF(clazz.sourceModule))
        }
      }
    } catch {
      case ex: TypeError =>
        if (!reporter.hasErrors) throw ex
    }

    if (phase.id <= currentRun.typerPhase.id) {
      treeCopy.Template(templ, templ.parents, templ.self,
        if (ts.isEmpty) templ.body else templ.body ++ ts // avoid copying templ.body if empty
      )
    }
    else templ
  }
}
