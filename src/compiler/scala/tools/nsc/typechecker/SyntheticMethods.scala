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

  /** Add the synthetic methods to case classes.
   */
  def addSyntheticMethods(templ: Template, clazz0: Symbol, context: Context): Template = {
    if (phase.erasedTypes)
      return templ

    val synthesizer = new ClassMethodSynthesis(
      clazz0,
      newTyper( if (reporter.hasErrors) context makeSilent false else context )
    )
    import synthesizer._

    val originalAccessors = clazz.caseFieldAccessors
    // private ones will have been renamed -- make sure they are entered
    // in the original order.
    def accessors = clazz.caseFieldAccessors sortBy { acc =>
      originalAccessors indexWhere { orig =>
        (acc.name == orig.name) || (acc.name startsWith (orig.name append "$"))
      }
    }
    val arity = accessors.size
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
      if (opt.experimental) {
        global.weakLub(accessors map (_.tpe.finalResultType))._1 match {
          case RefinedType(parents, decls) if !decls.isEmpty => intersectionType(parents)
          case tp                                            => tp
        }
      }
      else AnyClass.tpe
    )

    def forwardToRuntime(method: Symbol): Tree =
      forwardMethod(method, getMember(ScalaRunTimeModule, method.name prepend "_"))(This(clazz) :: _)

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
      clazzMember(newTermName(name)).info match {
        case NullaryMethodType(ConstantType(Constant(value))) => value.asInstanceOf[T]
        case _                                                => default
      }
    }
    def productIteratorMethod = {
      createMethod(nme.productIterator, iteratorOfType(accessorLub))(_ =>
        gen.mkMethodCall(ScalaRunTimeModule, nme.typedProductIterator, List(accessorLub), List(This(clazz)))
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
    def canEqualMethod: Tree = (
      createMethod(nme.canEqual_, List(AnyClass.tpe), BooleanClass.tpe)(m => 
        Ident(m.firstParam) IS_OBJ classExistentialType(clazz))
    )

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
      val arg0      = Ident(m.firstParam)
      val thatTest  = gen.mkIsInstanceOf(arg0, classExistentialType(clazz), true, false)
      val thatCast  = gen.mkCast(arg0, clazz.tpe)

      def argsBody: Tree = {
        val otherName = context.unit.freshTermName(clazz.name + "$")
        val otherSym  = m.newValue(otherName, m.pos, SYNTHETIC) setInfo clazz.tpe
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
      // Object_equals   -> (() => createMethod(Object_equals)(m => This(clazz) ANY_EQ Ident(m.firstParam)))
    )

    /** If you serialize a singleton and then deserialize it twice,
     *  you will have two instances of your singleton unless you implement
     *  readResolve.  Here it is implemented for all objects which have
     *  no implementation and which are marked serializable (which is true
     *  for all case objects.)
     */
    def needsReadResolve = (
         clazz.isModuleClass
      && clazz.isSerializable
      && !hasConcreteImpl(nme.readResolve)
    )

    def synthesize(): List[Tree] = {
      val methods = (
        if (!clazz.isCase) Nil
        else if (clazz.isModuleClass) caseObjectMethods
        else caseClassMethods
      )
      def impls = for ((m, impl) <- methods ; if !hasOverridingImplementation(m)) yield impl()
      def extras = (
        if (needsReadResolve) {
          // Aha, I finally decoded the original comment.
          // This method should be generated as private, but apparently if it is, then
          // it is name mangled afterward.  (Wonder why that is.) So it's only protected.
          // For sure special methods like "readResolve" should not be mangled.
          List(createMethod(nme.readResolve, Nil, ObjectClass.tpe)(m => { m setFlag PRIVATE ; REF(clazz.sourceModule) }))
        }
        else Nil
      )

      try impls ++ extras
      catch { case _: TypeError if reporter.hasErrors => Nil }
    }

    /** If this case class has any less than public accessors,
     *  adds new accessors at the correct locations to preserve ordering.
     *  Note that this must be done before the other method synthesis
     *  because synthesized methods need refer to the new symbols.
     *  Care must also be taken to preserve the case accessor order.
     */
    def caseTemplateBody(): List[Tree] = {
      val lb = ListBuffer[Tree]()
      def isRewrite(sym: Symbol) = sym.isCaseAccessorMethod && !sym.isPublic

      for (ddef @ DefDef(_, _, _, _, _, _) <- templ.body ; if isRewrite(ddef.symbol)) {
        val original = ddef.symbol
        val newAcc = deriveMethod(ddef.symbol, name => context.unit.freshTermName(name + "$")) { newAcc =>
          newAcc.makePublic
          newAcc resetFlag (ACCESSOR | PARAMACCESSOR)
          ddef.rhs.duplicate
        }
        ddef.symbol resetFlag CASEACCESSOR
        lb += logResult("case accessor new")(newAcc)
      }

      lb ++= templ.body ++= synthesize() toList
    }

    if (phase.id > currentRun.typerPhase.id) templ
    else treeCopy.Template(templ, templ.parents, templ.self,
      if (clazz.isCase) caseTemplateBody()
      else synthesize() match {
        case Nil  => templ.body // avoiding unnecessary copy
        case ms   => templ.body ++ ms
      }
    )
  }
}
