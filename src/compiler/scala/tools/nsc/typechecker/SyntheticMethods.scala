/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.typechecker

import symtab.Flags
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
trait SyntheticMethods extends ast.TreeDSL {
  self: Analyzer =>

  import global._                  // the global environment
  import definitions._             // standard classes and methods

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
  def addSyntheticMethods(templ: Template, clazz: Symbol, context: Context): Template = {

    val localContext  = if (reporter.hasErrors) context makeSilent false else context
    val localTyper    = newTyper(localContext)

    def hasImplementation(name: Name): Boolean = {
      val sym = clazz.info member name // member and not nonPrivateMember: bug #1385
      sym.isTerm && !(sym hasFlag DEFERRED)
    }

    def hasOverridingImplementation(meth: Symbol): Boolean = {
      val sym = clazz.info nonPrivateMember meth.name
      sym.alternatives exists { sym =>
        sym != meth && !(sym hasFlag DEFERRED) && !(sym hasFlag (SYNTHETIC | SYNTHETICMETH)) &&
        (clazz.thisType.memberType(sym) matches clazz.thisType.memberType(meth))
      }
    }

    def syntheticMethod(name: Name, flags: Int, tpeCons: Symbol => Type) =
      newSyntheticMethod(name, flags | OVERRIDE, tpeCons)

    def newSyntheticMethod(name: Name, flags: Int, tpeCons: Symbol => Type) = {
      val method = clazz.newMethod(clazz.pos, name) setFlag (flags | SYNTHETICMETH)
      method setInfo tpeCons(method)
      clazz.info.decls.enter(method).asInstanceOf[TermSymbol]
    }

    def makeNoArgConstructor(res: Type) =
      (sym: Symbol) => MethodType(Nil, res)
    def makeTypeConstructor(args: List[Type], res: Type) =
      (sym: Symbol) => MethodType(sym newSyntheticValueParams args, res)

    import CODE._

    def productPrefixMethod: Tree = typer.typed {
      val method = syntheticMethod(nme.productPrefix, 0, sym => PolyType(Nil, StringClass.tpe))
      DEF(method) === LIT(clazz.name.decode)
    }

    def productArityMethod(nargs: Int): Tree = {
      val method = syntheticMethod(nme.productArity, 0, sym => PolyType(Nil, IntClass.tpe))
      typer typed { DEF(method) === LIT(nargs) }
    }

    def productElementMethod(accs: List[Symbol]): Tree = {
      val symToTpe  = makeTypeConstructor(List(IntClass.tpe), AnyClass.tpe)
      val method    = syntheticMethod(nme.productElement, 0, symToTpe)
      val arg       = method ARG 0
      val default   = List( DEFAULT ==> THROW(IndexOutOfBoundsExceptionClass, arg) )
      val cases     =
        for ((sym, i) <- accs.zipWithIndex) yield
          CASE(LIT(i)) ==> Ident(sym)

      typer typed {
        DEF(method) === {
          arg MATCH { cases ::: default : _* }
        }
      }
    }

    def moduleToStringMethod: Tree = {
      val method = syntheticMethod(nme.toString_, FINAL, makeNoArgConstructor(StringClass.tpe))
      typer typed { DEF(method) === LIT(clazz.name.decode) }
    }

    def forwardingMethod(name: Name): Tree = {
      val target      = getMember(ScalaRunTimeModule, "_" + name)
      val paramtypes  = target.tpe.paramTypes drop 1
      val method      = syntheticMethod(
        name, 0, makeTypeConstructor(paramtypes, target.tpe.resultType)
      )

      typer.typed {
        DEF(method) === {
          Apply(gen.mkAttributedRef(target), This(clazz) :: (method ARGNAMES))
        }
      }
    }

    def equalsSym = syntheticMethod(
      nme.equals_, 0, makeTypeConstructor(List(AnyClass.tpe), BooleanClass.tpe)
    )

    /** The equality method for case modules:
     *   def equals(that: Any) = this eq that
     */
    def equalsModuleMethod: Tree = localTyper typed {
      val method = equalsSym
      val that = method ARG 0

      localTyper typed {
        DEF(method) === {
          (This(clazz) DOT Object_eq)(that AS AnyRefClass.tpe)
        }
      }
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
      val that = method ARG 0
      val constrParamTypes = clazz.primaryConstructor.tpe.paramTypes

      // returns (Apply, Bind)
      def makeTrees(acc: Symbol, cpt: Type): (Tree, Bind) = {
        val varName             = context.unit.fresh.newName(clazz.pos, acc.name + "$")
        val (eqMethod, binding) =
          if (cpt.isVarargs)  (nme.sameElements, Star(WILD))
          else                (nme.EQ          , WILD      )

        ((varName DOT eqMethod)(Ident(acc)), varName BIND binding)
      }

      // Creates list of parameters and a guard for each
      val (guards, params) = List.map2(clazz.caseFieldAccessors, constrParamTypes)(makeTrees) unzip

      // Pattern is classname applied to parameters, and guards are all logical and-ed
      val (guard, pat) = (AND(guards : _*), clazz.name.toTermName APPLY params)

      localTyper typed {
        DEF(method) === {
          (This(clazz) EQREF that) OR (that MATCH(
            (CASE(pat) IF guard)  ==> TRUE        ,
            DEFAULT               ==> FALSE
          ))
        }
      }
    }

    def hasSerializableAnnotation(clazz: Symbol) =
      clazz hasAnnotation SerializableAttr
    def isPublic(sym: Symbol) =
      !sym.hasFlag(PRIVATE | PROTECTED) && sym.privateWithin == NoSymbol

    def readResolveMethod: Tree = {
      // !!! the synthetic method "readResolve" should be private, but then it is renamed !!!
      val method = newSyntheticMethod(nme.readResolve, PROTECTED, makeNoArgConstructor(ObjectClass.tpe))
      typer typed {
        DEF(method) === gen.mkAttributedRef(clazz.sourceModule)
      }
    }

    def newAccessorMethod(tree: Tree): Tree = tree match {
      case DefDef(_, _, _, _, _, rhs) =>
        var newAcc = tree.symbol.cloneSymbol
        newAcc.name = context.unit.fresh.newName(tree.symbol.pos, tree.symbol.name + "$")
        newAcc setFlag SYNTHETIC resetFlag (ACCESSOR | PARAMACCESSOR | PRIVATE)
        newAcc = newAcc.owner.info.decls enter newAcc
        val result = typer typed { DEF(newAcc) === rhs.duplicate }
        log("new accessor method " + result)
        result
    }

    val ts = new ListBuffer[Tree]

    if (!phase.erasedTypes) try {
      if (clazz hasFlag Flags.CASE) {
        val isTop = !(clazz.info.baseClasses.tail exists (_ hasFlag Flags.CASE))
        // case classes are implicitly declared serializable
        clazz addAnnotation AnnotationInfo(SerializableAttr.tpe, Nil, Nil)

        if (isTop) {
          for (stat <- templ.body) {
            if (stat.isDef && stat.symbol.isMethod && stat.symbol.hasFlag(CASEACCESSOR) && !isPublic(stat.symbol)) {
              ts += newAccessorMethod(stat)
              stat.symbol resetFlag CASEACCESSOR
            }
          }
        }

        // methods for case classes only
        def classMethods = List(
          Object_hashCode -> (() => forwardingMethod(nme.hashCode_)),
          Object_toString -> (() => forwardingMethod(nme.toString_)),
          Object_equals   -> (() => equalsClassMethod)
        )
        // methods for case objects only
        def objectMethods = List(
          Object_toString -> (() => moduleToStringMethod)
        )
        // methods for both classes and objects
        def everywhereMethods = {
          val accessors = clazz.caseFieldAccessors
          List(
            Product_productPrefix   -> (() => productPrefixMethod),
            Product_productArity    -> (() => productArityMethod(accessors.length)),
            Product_productElement  -> (() => productElementMethod(accessors))
          )
        }

        if (clazz.isModuleClass) {
          // if there's a synthetic method in a parent case class, override its equality
          // with eq (see #883)
          val otherEquals = clazz.info.nonPrivateMember(Object_equals.name)
          if (otherEquals.owner != clazz && (otherEquals hasFlag SYNTHETICMETH)) ts += equalsModuleMethod
        }

        val methods = (if (clazz.isModuleClass) objectMethods else classMethods) ++ everywhereMethods
        for ((m, impl) <- methods ; if !hasOverridingImplementation(m))
          ts += impl()
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

    val synthetics = ts.toList
    treeCopy.Template(
      templ, templ.parents, templ.self, if (synthetics.isEmpty) templ.body else templ.body ::: synthetics)
  }
}
