package scala.tools.nsc
package typechecker

import java.lang.Math.min
import symtab.Flags._
import scala.tools.nsc.util._
import scala.reflect.runtime.ReflectionUtils
import scala.collection.mutable.{ListBuffer, Map => MutableMap}
import scala.reflect.ClassTag
import scala.reflect.internal.util.Statistics
import scala.reflect.macros.util._
import scala.util.control.ControlThrowable
import scala.reflect.macros.runtime.AbortMacroException
import scala.reflect.runtime.{universe => ru}

/**
 *  Code to deal with macros, namely with:
 *    * Compilation of macro definitions
 *    * Expansion of macro applications
 *
 *  Say we have in a class C:
 *
 *    def foo[T](xs: List[T]): T = macro fooBar
 *
 *  Then fooBar needs to point to a static method of the following form:
 *
 *    def fooBar[T: c.WeakTypeTag] // type tag annotation is optional
 *           (c: scala.reflect.macros.Context)
 *           (xs: c.Expr[List[T]])
 *           : c.Expr[T] = {
 *      ...
 *    }
 *
 *  Then, if foo is called in qual.foo[Int](elems), where qual: D,
 *  the macro application is expanded to a reflective invocation of fooBar with parameters:
 *
 *    (simpleMacroContext{ type PrefixType = D; val prefix = qual })
 *    (Expr(elems))
 *    (TypeTag(Int))
 */
trait Macros extends scala.tools.reflect.FastTrack with Traces with Helpers {
  self: Analyzer =>

  import global._
  import definitions._
  import treeInfo.{isRepeatedParamType => _, isUntypedType => _, _}
  import MacrosStats._
  def globalSettings = global.settings

  /** `MacroImplBinding` and its companion module are responsible for
   *  serialization/deserialization of macro def -> impl bindings.
   *
   *  The first officially released version of macros persisted these bindings across compilation runs
   *  using a neat trick. The right-hand side of a macro definition (which contains a reference to a macro impl)
   *  was typechecked and then put verbatim into an annotation on the macro definition.
   *
   *  This solution is very simple, but unfortunately it's also lacking. If we use it, then
   *  signatures of macro defs become transitively dependent on scala-reflect.jar
   *  (because they refer to macro impls, and macro impls refer to scala.reflect.macros.Context defined in scala-reflect.jar).
   *  More details can be found in comments to https://issues.scala-lang.org/browse/SI-5940.
   *
   *  Therefore we have to avoid putting macro impls into binding pickles and come up with our own serialization format.
   *  Situation is further complicated by the fact that it's not enough to just pickle macro impl's class name and method name,
   *  because macro expansion needs some knowledge about the shape of macro impl's signature (which we can't pickle).
   *  Hence we precompute necessary stuff (e.g. the layout of type parameters) when compiling macro defs.
   */

  /** Represents all the information that a macro definition needs to know about its implementation.
   *  Includes a path to load the implementation via Java reflection,
   *  and various accounting information necessary when composing an argument list for the reflective invocation.
   */
  private case class MacroImplBinding(
    // Is this macro impl a bundle (a trait extending Macro) or a vanilla def?
    val isBundle: Boolean,
    // Java class name of the class that contains the macro implementation
    // is used to load the corresponding object with Java reflection
    val className: String,
    // method name of the macro implementation
    // `className` and `methName` are all we need to reflectively invoke a macro implementation
    // because macro implementations cannot be overloaded
    val methName: String,
    // flattens the macro impl's parameter lists having symbols replaced with their fingerprints
    // currently fingerprints are calculated solely from types of the symbols:
    //   * c.Expr[T] => IMPLPARAM_EXPR
    //   * c.Tree => IMPLPARAM_TREE
    //   * c.WeakTypeTag[T] => index of the type parameter corresponding to that type tag
    //   * everything else (e.g. scala.reflect.macros.Context) => IMPLPARAM_OTHER
    // f.ex. for: def impl[T: WeakTypeTag, U, V: WeakTypeTag](c: Context)(x: c.Expr[T], y: c.Tree): (U, V) = ???
    // `signature` will be equal to List(List(-1), List(-1, -2), List(0, 2))
    val signature: List[List[Int]],
    // type arguments part of a macro impl ref (the right-hand side of a macro definition)
    // these trees don't refer to a macro impl, so we can pickle them as is
    val targs: List[Tree])

  private final val IMPLPARAM_TAG = 0 // actually it's zero and above, this is just a lower bound for >= checks
  private final val IMPLPARAM_OTHER = -1
  private final val IMPLPARAM_EXPR = -2
  private final val IMPLPARAM_TREE = -3

  /** Macro def -> macro impl bindings are serialized into a `macroImpl` annotation
   *  with synthetic content that carries the payload described in `MacroImplBinding`.
   *
   *  For example, for a pair of macro definition and macro implementation:
   *    def impl(c: scala.reflect.macros.Context): c.Expr[Unit] = c.literalUnit;
   *    def foo: Unit = macro impl
   *
   *  We will have the following annotation added on the macro definition `foo`:
   *
   *    @scala.reflect.macros.internal.macroImpl(
   *      `macro`(
   *        "signature" = List(-1),
   *        "methodName" = "impl",
   *        "versionFormat" = 1,
   *        "className" = "Macros$"))
   */
  private object MacroImplBinding {
    val versionFormat = 2

    def pickleAtom(obj: Any): Tree =
      obj match {
        case list: List[_] => Apply(Ident(ListModule), list map pickleAtom)
        case s: String => Literal(Constant(s))
        case i: Int => Literal(Constant(i))
        case b: Boolean => Literal(Constant(b))
      }

    def unpickleAtom(tree: Tree): Any =
      tree match {
        case Apply(list @ Ident(_), args) if list.symbol == ListModule => args map unpickleAtom
        case Literal(Constant(s: String)) => s
        case Literal(Constant(i: Int)) => i
        case Literal(Constant(b: Boolean)) => b
      }

    def pickle(macroImplRef: Tree): Tree = {
      val MacroImplReference(isBundle, owner, macroImpl, targs) = macroImplRef

      // todo. refactor when fixing SI-5498
      def className: String = {
        def loop(sym: Symbol): String = sym match {
          case sym if sym.isTopLevel =>
            val suffix = if (sym.isModuleClass) "$" else ""
            sym.fullName + suffix
          case sym =>
            val separator = if (sym.owner.isModuleClass) "" else "$"
            loop(sym.owner) + separator + sym.javaSimpleName.toString
        }

        loop(owner)
      }

      def signature: List[List[Int]] = {
        def fingerprint(tpe: Type): Int = tpe.dealias match {
          case TypeRef(_, RepeatedParamClass, underlying :: Nil) => fingerprint(underlying)
          case ExprClassOf(_) => IMPLPARAM_EXPR
          case TreeType() => IMPLPARAM_TREE
          case _ => IMPLPARAM_OTHER
        }

        val transformed = transformTypeTagEvidenceParams(macroImplRef, (param, tparam) => tparam)
        mmap(transformed)(p => if (p.isTerm) fingerprint(p.info) else p.paramPos)
      }

      val payload = List[(String, Any)](
        "versionFormat" -> versionFormat,
        "isBundle"      -> isBundle,
        "className"     -> className,
        "methodName"    -> macroImpl.name.toString,
        "signature"     -> signature
      )

      // the shape of the nucleus is chosen arbitrarily. it doesn't carry any payload.
      // it's only necessary as a stub `fun` for an Apply node that carries metadata in its `args`
      // so don't try to find a program element named "macro" that corresponds to the nucleus
      // I just named it "macro", because it's macro-related, but I could as well name it "foobar"
      val nucleus = Ident(newTermName("macro"))
      val wrapped = Apply(nucleus, payload map { case (k, v) => Assign(pickleAtom(k), pickleAtom(v)) })
      val pickle = gen.mkTypeApply(wrapped, targs map (_.duplicate))

      // assign NoType to all freshly created AST nodes
      // otherwise pickler will choke on tree.tpe being null
      // there's another gotcha
      // if you don't assign a ConstantType to a constant
      // then pickling will crash
      new Transformer {
        override def transform(tree: Tree) = {
          tree match {
            case Literal(const @ Constant(x)) if tree.tpe == null => tree setType ConstantType(const)
            case _ if tree.tpe == null => tree setType NoType
            case _ => ;
          }
          super.transform(tree)
        }
      }.transform(pickle)
    }

    def unpickle(pickle: Tree): MacroImplBinding = {
      val (wrapped, targs) =
        pickle match {
          case TypeApply(wrapped, targs) => (wrapped, targs)
          case wrapped => (wrapped, Nil)
        }
      val Apply(_, pickledPayload) = wrapped
      val payload = pickledPayload.map{ case Assign(k, v) => (unpickleAtom(k), unpickleAtom(v)) }.toMap

      val pickleVersionFormat = payload("versionFormat").asInstanceOf[Int]
      if (versionFormat != pickleVersionFormat) throw new Error("macro impl binding format mismatch: expected $versionFormat, actual $pickleVersionFormat")

      val isBundle = payload("isBundle").asInstanceOf[Boolean]
      val className = payload("className").asInstanceOf[String]
      val methodName = payload("methodName").asInstanceOf[String]
      val signature = payload("signature").asInstanceOf[List[List[Int]]]
      MacroImplBinding(isBundle, className, methodName, signature, targs)
    }
  }

  private def bindMacroImpl(macroDef: Symbol, macroImplRef: Tree): Unit = {
    val pickle = MacroImplBinding.pickle(macroImplRef)
    macroDef withAnnotation AnnotationInfo(MacroImplAnnotation.tpe, List(pickle), Nil)
  }

  private def loadMacroImplBinding(macroDef: Symbol): MacroImplBinding = {
    val Some(AnnotationInfo(_, List(pickle), _)) = macroDef.getAnnotation(MacroImplAnnotation)
    MacroImplBinding.unpickle(pickle)
  }

  def computeMacroDefTypeFromMacroImplRef(macroDdef: DefDef, macroImplRef: Tree): Type = {
    macroImplRef match {
      case MacroImplReference(_, _, macroImpl, targs) =>
        // Step I. Transform c.Expr[T] to T and c.Tree to <untyped>
        var runtimeType = decreaseMetalevel(macroImpl.info.finalResultType)

        // Step II. Transform type parameters of a macro implementation into type arguments in a macro definition's body
        runtimeType = runtimeType.substituteTypes(macroImpl.typeParams, targs map (_.tpe))

        // Step III. Transform c.prefix.value.XXX to this.XXX and implParam.value.YYY to defParam.YYY
        def unsigma(tpe: Type): Type =
          transformTypeTagEvidenceParams(macroImplRef, (param, tparam) => NoSymbol) match {
            case (implCtxParam :: Nil) :: implParamss =>
              val implToDef = flatMap2(implParamss, macroDdef.vparamss)(map2(_, _)((_, _))).toMap
              object UnsigmaTypeMap extends TypeMap {
                def apply(tp: Type): Type = tp match {
                  case TypeRef(pre, sym, args) =>
                    val pre1 = pre match {
                      case SingleType(SingleType(SingleType(NoPrefix, c), prefix), value) if c == implCtxParam && prefix == MacroContextPrefix && value == ExprValue =>
                        ThisType(macroDdef.symbol.owner)
                      case SingleType(SingleType(NoPrefix, implParam), value) if value == ExprValue =>
                        implToDef get implParam map (defParam => SingleType(NoPrefix, defParam.symbol)) getOrElse pre
                      case _ =>
                        pre
                    }
                    val args1 = args map mapOver
                    TypeRef(pre1, sym, args1)
                  case _ =>
                    mapOver(tp)
                }
              }

              UnsigmaTypeMap(tpe)
            case _ =>
              tpe
          }

        unsigma(runtimeType)
      case _ =>
        ErrorType
    }
  }

  /** Verifies that the body of a macro def typechecks to a reference to a static public non-overloaded method or a top-level macro bundle,
   *  and that that method is signature-wise compatible with the given macro definition.
   *
   *  @return Macro impl reference for the given macro definition if everything is okay.
   *          EmptyTree if an error occurs.
   */
  def typedMacroBody(typer: Typer, macroDdef: DefDef): Tree = {
    val macroDef = macroDdef.symbol
    assert(macroDef.isMacro, macroDdef)

    macroLogVerbose("typechecking macro def %s at %s".format(macroDef, macroDdef.pos))
    if (fastTrack contains macroDef) {
      macroLogVerbose("typecheck terminated unexpectedly: macro is fast track")
      assert(!macroDdef.tpt.isEmpty, "fast track macros must provide result type")
      EmptyTree
    } else {
      def fail() = { if (macroDef != null) macroDef setFlag IS_ERROR; macroDdef setType ErrorType; EmptyTree }
      def success(macroImplRef: Tree) = { bindMacroImpl(macroDef, macroImplRef); macroImplRef }

      if (!typer.checkFeature(macroDdef.pos, MacrosFeature, immediate = true)) {
        macroLogVerbose("typecheck terminated unexpectedly: language.experimental.macros feature is not enabled")
        fail()
      } else {
        inferImplicit(macroDdef, Predef_MacroCompiler.tpe, isView = false, context = typer.context,
                      reportAmbiguous = true, saveAmbiguousDivergent = true) match {
          case notFound if notFound.tree.isEmpty =>
            fail()
          case found =>
            val macroImplRef = typer.typed(Apply(Select(found.tree, nme.resolveMacroImpl), List(macroDdef)))
            if (macroImplRef.isErroneous) fail() else success(macroImplRef)
        }
      }
    }
  }

  /** Macro classloader that is used to resolve and run macro implementations.
   *  Loads classes from from -cp (aka the library classpath).
   *  Is also capable of detecting REPL and reusing its classloader.
   */
  lazy val macroClassloader: ClassLoader = {
    val classpath = global.classPath.asURLs
    macroLogVerbose("macro classloader: initializing from -cp: %s".format(classpath))
    val loader = ScalaClassLoader.fromURLs(classpath, self.getClass.getClassLoader)

    // a heuristic to detect the REPL
    if (global.settings.exposeEmptyPackage.value) {
      macroLogVerbose("macro classloader: initializing from a REPL classloader".format(global.classPath.asURLs))
      import scala.tools.nsc.interpreter._
      val virtualDirectory = global.settings.outputDirs.getSingleOutput.get
      new AbstractFileClassLoader(virtualDirectory, loader) {}
    } else {
      loader
    }
  }

  /** Reflective mirror built from `macroClassloader`.
   */
  private lazy val macroMirror: ru.JavaMirror = ru.runtimeMirror(macroClassloader)

  /** Flavors of macro runtime, varying based on what the compiler wants from a macro.
   */
  type MacroRuntimeFlavor = Int
  final val FLAVOR_EXPAND: MacroRuntimeFlavor = 1
  final val FLAVOR_ONINFER: MacroRuntimeFlavor = 2
  private val flavorNames = Map(FLAVOR_EXPAND -> "expand", FLAVOR_ONINFER -> "onInfer")

  /** Produces a function that can be used to invoke macro implementation for a given macro definition:
   *    1) Looks up macro implementation symbol in this universe.
   *    2) Loads its enclosing class from the macro classloader.
   *    3) Loads the companion of that enclosing class from the macro classloader.
   *    4) Resolves macro implementation within the loaded companion.
   *
   *  @return Requested runtime if macro implementation can be loaded successfully from either of the mirrors,
   *          `null` otherwise.
   */
  type MacroRuntime = MacroArgs => Any
  private val macroRuntimesCache = perRunCaches.newWeakMap[Symbol, MutableMap[MacroRuntimeFlavor, MacroRuntime]]
  def macroRuntime(macroDef: Symbol, flavor: MacroRuntimeFlavor): MacroRuntime = {
    macroTraceVerbose(s"looking for ${flavorNames(flavor)} macro runtime: ")(macroDef)
    if (fastTrack contains macroDef) {
      if (flavor == FLAVOR_EXPAND) {
        macroLogVerbose("macro expansion is serviced by a fast track")
        fastTrack(macroDef)
      } else {
        macroLogVerbose(s"${flavorNames(flavor)} for a fast track macro => ignoring")
        null
      }
    } else {
      val symbolRuntimesCache = macroRuntimesCache.getOrElseUpdate(macroDef, MutableMap())
      symbolRuntimesCache.getOrElseUpdate(flavor, {
        val binding = loadMacroImplBinding(macroDef)
        val isBundle = binding.isBundle
        val className = binding.className
        val methName =
          if (flavor == FLAVOR_EXPAND) binding.methName
          else if (flavor == FLAVOR_ONINFER) "onInfer"
          else abort(s"${flavorNames(flavor)} $macroDef")
        macroLogVerbose(s"resolved implementation as $className.$methName")

        try {
          // JAVA REFLECTION
          // TODO: get rid of this when the Scala reflection-based version is fixed
          // ==========
          macroTraceVerbose("loading implementation class: ")(className)
          macroTraceVerbose("classloader is: ")(ReflectionUtils.show(macroClassloader))
          val implClass = Class.forName(className, true, macroClassloader)
          val implMeths = implClass.getDeclaredMethods.find(_.getName == methName)
          // relies on the fact that macro impls cannot be overloaded
          // so every methName can resolve to at maximum one method
          val implMeth = implMeths getOrElse { throw new NoSuchMethodException(s"$className.$methName") }
          macroLogVerbose("successfully loaded macro impl as (%s, %s)".format(implClass, implMeth))
          args => {
            val implObj =
              if (isBundle) implClass.getConstructor(classOf[scala.reflect.macros.Context]).newInstance(args.c)
              else implClass.getField("MODULE$").get(null)
            val implArgs = if (isBundle) args.others else args.c +: args.others
            implMeth.invoke(implObj, implArgs.asInstanceOf[Seq[AnyRef]]: _*)
          }
          // SCALA REFLECTION
          // TODO: make it work under partest, where thread-unsafety of reflection raises it's ugly head
          // ==========
          // macroTraceVerbose("loading implementation class: ")(className)
          // macroTraceVerbose("classloader is: ")(ReflectionUtils.show(macroClassloader))
          // val implContainerSym = macroMirror.classSymbol(Class.forName(className, true, macroClassloader))
          // val implMethSym = implContainerSym.typeSignature.member(ru.TermName(methName)).asMethod
          // macroLogVerbose(s"successfully loaded macro impl as ($implContainerSym, $implMethSym)")
          // args => {
          //   val implContainer =
          //     if (isBundle) {
          //       val implCtorSym = implContainerSym.typeSignature.member(ru.nme.CONSTRUCTOR).asMethod
          //       macroMirror.reflectClass(implContainerSym).reflectConstructor(implCtorSym)(args.c)
          //     } else {
          //       macroMirror.reflectModule(implContainerSym.module.asModule).instance
          //     }
          //   val implMeth = macroMirror.reflect(implContainer).reflectMethod(implMethSym)
          //   val implArgs = if (isBundle) args.others else args.c +: args.others
          //   implMeth(implArgs: _*)
          // }
        } catch {
          case ex: Exception =>
            macroTraceVerbose(s"macro runtime failed to load: ")(ex.toString)
            if (flavor == FLAVOR_EXPAND) macroDef setFlag IS_ERROR
            null
        }
      })
    }
  }

  def macroContext(typer: Typer, prefixTree: Tree, expandeeTree: Tree): MacroContext = {
    new {
      val universe: self.global.type = self.global
      val callsiteTyper: universe.analyzer.Typer = typer.asInstanceOf[global.analyzer.Typer]
      val expandee = universe.analyzer.macroExpanderAttachment(expandeeTree).original orElse expandeeTree
      val macroRole = universe.analyzer.macroExpanderAttachment(expandeeTree).role
    } with UnaffiliatedMacroContext {
      val prefix = Expr[Nothing](prefixTree)(TypeTag.Nothing)
      override def toString = "MacroContext(%s@%s +%d)".format(expandee.symbol.name, expandee.pos, enclosingMacros.length - 1 /* exclude myself */)
    }
  }

  /** Calculate the arguments to pass to a macro implementation when expanding the provided tree.
   */
  case class MacroArgs(c: MacroContext, others: List[Any])

  private def macroArgs(typer: Typer, expandee: Tree): MacroArgs = {
    val macroDef = expandee.symbol
    val paramss = macroDef.paramss
    val treeInfo.Applied(core, targs, argss) = expandee
    val prefix = core match { case Select(qual, name) => qual; case _ => EmptyTree }
    val context = expandee.attachments.get[MacroRuntimeAttachment].flatMap(_.macroContext).getOrElse(macroContext(typer, prefix, expandee))

    macroTraceVerbose("context: ")(context)
    macroTraceVerbose("targs: ")(targs)
    macroTraceVerbose("argss: ")(argss)
    macroTraceVerbose("paramss: ")(paramss)

    import typer.TyperErrorGen._
    val isNullaryArgsEmptyParams = argss.isEmpty && paramss == ListOfNil
    if (paramss.length < argss.length) MacroTooManyArgumentListsError(expandee)
    if (paramss.length > argss.length && !isNullaryArgsEmptyParams) MacroTooFewArgumentListsError(expandee)

    val macroImplArgs: List[Any] =
      if (fastTrack contains macroDef) {
        // Take a dry run of the fast track implementation
        if (fastTrack(macroDef) validate expandee) argss.flatten
        else MacroTooFewArgumentListsError(expandee)
      }
      else {
        val binding = loadMacroImplBinding(macroDef)
        val signature = if (binding.isBundle) binding.signature else binding.signature.tail
        macroTraceVerbose("binding: ")(binding)

        // STEP I: prepare value arguments of the macro expansion
        // wrap argss in c.Expr if necessary (i.e. if corresponding macro impl param is of type c.Expr[T])
        // expand varargs (nb! varargs can apply to any parameter section, not necessarily to the last one)
        val trees = map3(argss, paramss, signature)((args, defParams, implParams) => {
          val isVarargs = isVarArgsList(defParams)
          if (isVarargs) {
            if (defParams.length > args.length + 1) MacroTooFewArgumentsError(expandee)
          } else {
            if (defParams.length < args.length) MacroTooManyArgumentsError(expandee)
            if (defParams.length > args.length) MacroTooFewArgumentsError(expandee)
          }

          val wrappedArgs = mapWithIndex(args)((arg, j) => {
            val fingerprint = implParams(min(j, implParams.length - 1))
            fingerprint match {
              case IMPLPARAM_EXPR => context.Expr[Nothing](arg)(TypeTag.Nothing) // TODO: SI-5752
              case IMPLPARAM_TREE => arg
              case _ => abort(s"unexpected fingerprint $fingerprint in $binding with paramss being $paramss " +
                              s"corresponding to arg $arg in $argss")
            }
          })

          if (isVarargs) {
            val (normal, varargs) = wrappedArgs splitAt (defParams.length - 1)
            normal :+ varargs // pack all varargs into a single Seq argument (varargs Scala style)
          } else wrappedArgs
        })
        macroTraceVerbose("trees: ")(trees)

        // STEP II: prepare type arguments of the macro expansion
        // if paramss have typetag context bounds, add an arglist to argss if necessary and instantiate the corresponding evidences
        // consider the following example:
        //
        //   class D[T] {
        //     class C[U] {
        //       def foo[V] = macro Impls.foo[T, U, V]
        //     }
        //   }
        //
        //   val outer1 = new D[Int]
        //   val outer2 = new outer1.C[String]
        //   outer2.foo[Boolean]
        //
        // then T and U need to be inferred from the lexical scope of the call using `asSeenFrom`
        // whereas V won't be resolved by asSeenFrom and need to be loaded directly from `expandee` which needs to contain a TypeApply node
        // also, macro implementation reference may contain a regular type as a type argument, then we pass it verbatim
        val tags = signature.flatten filter (_ >= IMPLPARAM_TAG) map (paramPos => {
          val targ = binding.targs(paramPos).tpe.typeSymbol
          val tpe = if (targ.isTypeParameterOrSkolem) {
            if (targ.owner == macroDef) {
              // doesn't work when macro def is compiled separately from its usages
              // then targ is not a skolem and isn't equal to any of macroDef.typeParams
              // val argPos = targ.deSkolemize.paramPos
              val argPos = macroDef.typeParams.indexWhere(_.name == targ.name)
              targs(argPos).tpe
            } else
              targ.tpe.asSeenFrom(
                if (prefix == EmptyTree) macroDef.owner.tpe else prefix.tpe,
                macroDef.owner)
          } else
            targ.tpe
          context.WeakTypeTag(tpe)
        })
        macroTraceVerbose("tags: ")(tags)

        // if present, tags always come in a separate parameter/argument list
        // that's because macro impls can't have implicit parameters other than c.WeakTypeTag[T]
        (trees :+ tags).flatten
      }
    MacroArgs(context, macroTraceVerbose("macroImplArgs: ")(macroImplArgs))
  }

  /** Keeps track of macros in-flight.
   *  See more informations in comments to `openMacros` in `scala.reflect.macros.Context`.
   */
  private var _openMacros = List[MacroContext]()
  def openMacros = _openMacros
  private def pushMacroContext(c: MacroContext) = _openMacros ::= c
  private def popMacroContext() = _openMacros = _openMacros.tail
  def enclosingMacroPosition = openMacros map (_.macroApplication.pos) find (_ ne NoPosition) getOrElse NoPosition

  /** Describes the role that the macro expandee is performing.
   */
  type MacroRole = String
  final def APPLY_ROLE: MacroRole = "APPLY_ROLE"
  final def TYPE_ROLE: MacroRole = "TYPE_ROLE"
  final def APPLIED_TYPE_ROLE: MacroRole = "APPLIED_TYPE_ROLE"
  final def PARENT_ROLE: MacroRole = "PARENT_ROLE"
  final def NEW_ROLE: MacroRole = "NEW_ROLE"
  final def ANNOTATION_ROLE: MacroRole = "ANNOTATION_ROLE"
  private val roleNames = Map(
    APPLY_ROLE -> "apply", TYPE_ROLE -> "type", APPLIED_TYPE_ROLE -> "applied type",
    PARENT_ROLE -> "parent", NEW_ROLE -> "new", ANNOTATION_ROLE -> "annotation")

  /** Performs macro expansion:
   *
   *  ========= Expandable trees =========
   *
   *  A term of one of the following shapes:
   *
   *    Ident(<term macro>)
   *    Select(<any qualifier>, <term macro>)
   *    TypeApply(<any of the above>, <targs>)
   *    Apply(...Apply(<any of the above>, <args1>)...<argsN>)
   *
   *  A type of one of the following shapes:
   *
   *    Ident(<term macro>)
   *    SelectFromTypeTree(<any qualifier>, <term macro>)
   *    AppliedTypeTree(<any of the above>, <targs>)
   *    DependentTypeTree(...DependentTypeTree(<any of the above>, <args1>)...<argsN>)
   *
   *  Unlike term macros, macro types expand differently depending on the role they play.
   *  There is a total of 5 different roles for macro types:
   *    1) Type tree, as in `def x: TM(2)(3) = ???`
   *    2) Applied type tree, as in `TM(2)(3)[Int]`
   *    3) Parent, as in `class C extends TM(2)(3)` or `new TM(2)(3){}`
   *    4) New, as in `new TM(2)(3)`
   *    5) Annotation, as in `@TM(2)(3) class C`
   *
   *  ========= Macro expansion =========
   *
   *  First of all `macroExpandXXX`:
   *    1) If necessary desugars the `expandee` to fit into `macroExpand1`
   *
   *  Then `macroExpand1`:
   *    2) Checks whether the expansion needs to be delayed
   *    3) Loads macro implementation using `macroMirror`
   *    4) Synthesizes invocation arguments for the macro implementation
   *    5) Checks that the result is a tree or an expr bound to this universe
   *
   *  Finally `macroExpandXXX`:
   *    6) Validates the expansion against the white list of supported tree shapes
   *    7) Typechecks the result as required by the circumstances of the macro application
   *
   *  If -Ymacro-debug-lite is enabled, you will get basic notifications about macro expansion
   *  along with macro expansions logged in the form that can be copy/pasted verbatim into REPL.
   *
   *  If -Ymacro-debug-verbose is enabled, you will get detailed log of how exactly this function
   *  performs class loading and method resolution in order to load the macro implementation.
   *  The log will also include other non-trivial steps of macro expansion.
   *
   *  @return
   *    the expansion result                    if the expansion has been successful,
   *    the fallback tree                       if the expansion has been unsuccessful, but there is a fallback,
   *    the expandee unchanged                  if the expansion has been delayed,
   *    the expandee fully expanded             if the expansion has been delayed before and has been expanded now,
   *    the expandee with an error marker set   if the expansion has been cancelled due malformed arguments or implementation
   *    the expandee with an error marker set   if there has been an error
   */
  private abstract class MacroExpander[Result: ClassTag](val role: MacroRole, val typer: Typer, val expandee: Tree) {
    def allowExpandee(expandee: Tree): Boolean = true
    def allowExpanded(expanded: Tree): Boolean = true
    def allowedExpansions: String = "anything"
    def allowResult(result: Result): Boolean = true

    def onSuccess(expanded: Tree): Result
    def onFallback(expanded: Tree): Result
    def onSuppressed(expandee: Tree): Result = expandee match { case expandee: Result => expandee }
    def onDelayed(expanded: Tree): Result = expanded match { case expanded: Result => expanded }
    def onSkipped(expanded: Tree): Result = expanded match { case expanded: Result => expanded }
    def onFailure(expanded: Tree): Result = { typer.infer.setError(expandee); expandee match { case expandee: Result => expandee } }

    def template: Option[Template] = None
    def apply(desugared: Tree): Result = {
      if (isMacroExpansionSuppressed(desugared)) onSuppressed(expandee)
      else expand(desugared)
    }

    protected def expand(desugared: Tree): Result = {
      def showDetailed(tree: Tree) = showRaw(tree, printIds = true, printTypes = true)
      def summary() = s"expander = $this, expandee = ${showDetailed(expandee)}, desugared = ${if (expandee == desugared) () else showDetailed(desugared)}"
      if (macroDebugVerbose) println(s"macroExpand: ${summary()}")
      assert(allowExpandee(expandee), summary())

      val start = if (Statistics.canEnable) Statistics.startTimer(macroExpandNanos) else null
      if (Statistics.canEnable) Statistics.incCounter(macroExpandCount)
      try {
        linkExpandeeAndDesugared(expandee, desugared, role, template)
        macroExpand1(typer, desugared) match {
          case Success(expanded) =>
            if (allowExpanded(expanded)) {
              // also see http://groups.google.com/group/scala-internals/browse_thread/thread/492560d941b315cc
              val expanded1 = try onSuccess(duplicateAndKeepPositions(expanded)) finally popMacroContext()
              if (!hasMacroExpansionAttachment(expanded1)) linkExpandeeAndExpanded(expandee, expanded1)
              if (allowResult(expanded1)) expanded1 else onFailure(expanded)
            } else {
              typer.TyperErrorGen.MacroInvalidExpansionError(expandee, roleNames(role), allowedExpansions)
              onFailure(expanded)
            }
          case Fallback(fallback) => onFallback(fallback)
          case Delayed(delayed) => onDelayed(delayed)
          case Skipped(skipped) => onSkipped(skipped)
          case Failure(failure) => onFailure(failure)
        }
      } finally {
        if (Statistics.canEnable) Statistics.stopTimer(macroExpandNanos, start)
      }
    }
  }

  /** Expands a tree that carries a term, which happens to be a term macro.
   *  @see MacroExpander
   */
   private abstract class TermMacroExpander(role: MacroRole, typer: Typer, expandee: Tree, mode: Mode, pt: Type)
                  extends MacroExpander[Tree](role, typer, expandee) {
      override def allowedExpansions: String = "term trees"
      override def allowExpandee(expandee: Tree) = expandee.isTerm
      override def onSuccess(expanded: Tree) = typer.typed(expanded, mode, pt)
      override def onFallback(fallback: Tree) = typer.typed(fallback, mode, pt)
   }

  /** Expands a term macro used in apply role as `M(2)(3)` in `val x = M(2)(3)`.
   *  @see MacroExpander
   */
  def macroExpandApply(typer: Typer, expandee: Tree, mode: Mode, pt: Type) = {
    object expander extends TermMacroExpander(APPLY_ROLE, typer, expandee, mode, pt) {
      override def onSuccess(expanded: Tree) = {
        // prematurely annotate the tree with a macro expansion attachment
        // so that adapt called indirectly by typer.typed knows that it needs to apply the existential fixup
        linkExpandeeAndExpanded(expandee, expanded)
        var expectedTpe = expandee.tpe
        if (isNullaryInvocation(expandee)) expectedTpe = expectedTpe.finalResultType
        // `macroExpandApply` is called from `adapt`, where implicit conversions are disabled
        // therefore we need to re-enable the conversions back temporarily
        if (macroDebugVerbose) println(s"typecheck #1 (against expectedTpe = $expectedTpe): $expanded")
        val expanded1 =
          if (isUntypedType(expectedTpe)) expanded
          else typer.context.withImplicitsEnabled(typer.typed(expanded, mode, expectedTpe))
        if (expanded1.isErrorTyped) {
          if (macroDebugVerbose) println(s"typecheck #1 has failed: ${typer.context.errBuffer}")
          expanded1
        } else {
          if (macroDebugVerbose) println(s"typecheck #2 (against pt = $pt): $expanded1")
          val expanded2 = typer.context.withImplicitsEnabled(super.onSuccess(expanded1))
          if (macroDebugVerbose && expanded2.isErrorTyped) println(s"typecheck #2 has failed: ${typer.context.errBuffer}")
          expanded2
        }
      }
    }
    expander(expandee)
  }

  /** Expands a tree that carries a type, which happens to be a macro type.
   *  @see MacroExpander
   */
  private abstract class MacroTypeExpander[Result: ClassTag](role: MacroRole, typer: Typer, original: Tree, val tpt: Tree, val targs: List[Tree], val argss: List[List[Tree]])
                 extends MacroExpander[Result](role, typer, original) {
    def isTypeLike: Boolean = false
    def isParentLike: Boolean = false
    def isTemplateLike: Boolean = false

    override def allowExpandee(expandee: Tree) =
      if (isTypeLike) expandee.isType
      else true // the shapes are too diverse to be easily validated

    override def allowedExpansions =
      if (isTypeLike) "Ident, Select, TypTrees except for TypeBoundsTree, and their Annotated versions"
      else if (isParentLike) "Apply, Ident, Select, class type TypTrees, and their Annotated versions"
      else if (isTemplateLike) "Template, Apply, Ident, Select, class type TypTrees, and their Annotated versions"
      else abort("the expander should either be type-like, parent-like or template-like")

    override def allowExpanded(expanded: Tree): Boolean = expanded match {
      case Template(_, _, _) => isTemplateLike
      case Apply(_, _) => isTemplateLike || isParentLike
      case Annotated(_, Apply(_, _)) => isTemplateLike || isParentLike
      case Ident(_) => true
      case Select(_, _) => true
      case Annotated(_, annottee) => allowExpanded(annottee)
      case TypeBoundsTree(_, _) => false
      case _: TypTree => true
      case _ => false
    }

    def allowResultTree(result: Tree): Boolean = {
      if (result.tpe == null || result.isErrorTyped) true
      else allowResultType(result.tpe)
    }

    def allowResultAnn(result: AnnotationInfo): Boolean = {
      if (result.isErroneous || result == ErroneousAnnotation) true
      else allowResultType(result.atp)
    }

    def allowResultType(result: Type): Boolean = {
      // it's incorrect to check only nextOverriddenSymbol, because by the virtue of mixin composition
      // we might end up overriding an unrelated symbol, e.g. as in macro-override-abstract-overrides-macro
      val pre = tpt match { case ref: RefTree => ref.qualifier }
      if (pre != EmptyTree) {
        pre.tpe.baseClasses.forall(base => {
          val overridden = tpt.symbol.macroType.overriddenSymbol(base)
          if (overridden.isAliasTypeNoKidding || overridden.isAbstractType) {
            val info = overridden.info.asSeenFrom(pre.tpe, overridden.owner)
            // TODO: unfortunately we also need to do some inference here, otherwise we're going to get errors like:
            // macro expansion C violates bounds  >: [U <: C]C <: [U <: C]C
            if (!(info.bounds containsType result)) {
              typer.TyperErrorGen.MacroTypeExpansionViolatesOverriddenBounds(expandee, result, overridden, info.bounds)
              false
            } else true
          } else true
        })
      } else true
    }

    override def onFallback(expanded: Tree): Result = abort("type macros aren't supposed to support fallback")

    override def onDelayed(expanded: Tree): Result = {
      // we can't delay treatment of not expanded macros here like we do with term macros
      // because callees in Typers expect `macroExpand<some type role>` to return a type tree
      typer.TyperErrorGen.MacroTypeHasntBeenExpandedError(expanded)
      onFailure(expandee)
    }

    // when `macroExpand1` for a macro type returns Skip(expanded)
    // it means that its desugaring (an application of a type macro) itself expanded into Skip(expanded)
    // which means that the expandee was first delayed, but later it did get expanded
    // for a macro type expander this is equivalent to success
    override def onSkipped(expanded: Tree): Result = onSuccess(expanded)

    def prepare(desugared: Tree): Tree

    override def expand(expandee: Tree): Result = {
      assert(tpt.symbol.isMacroType, "core of the macro type application should have been pre-typechecked in advance")
      val macroName = nme.typeMacroName(tpt.symbol.name)
      val macroRef = tpt match {
        case Select(qual, _) => Select(qual, macroName)
        case Ident(_) => Ident(macroName)
      }
      val desugared = atPos(original.pos)(gen.mkApply(macroRef, targs, argss))
      val desugared1 = unsuppressMacroExpansion(prepare(suppressMacroExpansion(desugared)))
      if (desugared1 == EmptyTree || desugared1.isErrorTyped) onFailure(desugared1)
      else super.expand(desugared1)
    }
  }

  /** Expands a macro type used in type role as `TM(2)(3)` in `def x: TM(2)(3) = ???`.
   *  @see MacroExpander
   */
  def macroExpandType(typer: Typer, expandee: Tree, mode: Mode, pt: Type) = {
    val treeInfo.Applied(tpt, targs, argss) = expandee
    object expander extends MacroTypeExpander[Tree](TYPE_ROLE, typer, expandee, tpt, targs, argss) {
      override def isTypeLike = true
      override def prepare(desugared: Tree) = typer.typed(desugared, EXPRmode, WildcardType)
      override def onSuccess(expanded: Tree) = typer.typed(expanded, mode, pt)
      override def allowResult(result: Tree) = allowResultTree(result)
    }
    expander(expandee)
  }

  /** Expands a macro type used in applied type role as `TM(2)(3)` in `TM(2)(3)[Int]`.
   *  @see MacroExpander
   */
  def macroExpandAppliedType(typer: Typer, expandee: Tree, mode: Mode) = {
    val treeInfo.Applied(tpt, targs, argss) = expandee
    object expander extends MacroTypeExpander[Tree](APPLIED_TYPE_ROLE, typer, expandee, tpt, targs, argss) {
      override def isTypeLike = true
      override def prepare(desugared: Tree) = typer.typed(desugared, EXPRmode, WildcardType)
      override def onSuccess(expanded: Tree) = typer.typed1(expanded, mode | FUNmode | TAPPmode, WildcardType)
      override def allowResult(result: Tree) = allowResultTree(result)
    }
    expander(expandee)
  }

  /** Expands a macro type used in parent role as `TM(2)(3)` in `class C extends TM(2)(3)` or `new TM(2)(3){}`.
   *  @see MacroExpander
   */
  def macroExpandParent(typer: Typer, original: Tree, tpt: Tree, targs: List[Tree], argss: List[List[Tree]], templ: Template, inMixinPosition: Boolean) = {
    object expander extends MacroTypeExpander[Tree](PARENT_ROLE, typer, original, tpt, targs, argss) {
      override def isTemplateLike = true
      override def template = Some(templ)
      override def prepare(desugared: Tree) = {
        if (typer.context.owner.isTrait) typer.typed(desugared, EXPRmode, WildcardType)
        else typer.typedPrimaryConstrBody(templ)(desugared)
      }
      override def onSuccess(expanded: Tree) = {
        val expanded1 = expanded match {
          // AnyRef emitted here is just a dummy that let's the compiler know
          // that the namer needs to replace the template being typechecked
          case templ1 @ Template(_, _, _) => Ident(AnyRefClass) updateAttachment MacroExpansionAttachment(original, expanded)
          case _ => expanded
        }
        typer.typedParentType(expanded1, templ, inMixinPosition)
      }
      override def allowResult(result: Tree) = allowResultTree(result)
    }
    expander(original)
  }

  /** Expands a macro type used in new role as `TM(2)(3)` in `new TM(2)(3)`.
   *  Contrast this role with parent role as in `new TM(2)(3){}`. In the former case, we get a New node,
   *  while in the latter case we get a Template for an anonymous class + a trivial New node.
   *  @see MacroExpander
   */
  def macroExpandNew(typer: Typer, original: Tree, tpt: Tree, targs: List[Tree], argss: List[List[Tree]], mode: Mode) = {
    // TODO: at the moment factories for New transform Nil arglists to ListOfNil
    // back then I tried to play with this, but apparently something in compiler's guts depends on this hack and won't give up
    // therefore we need to undo that hack here or otherwise nullary type macros won't be usable in new role
    val argss1 = if (argss == ListOfNil) Nil else argss
    object expander extends MacroTypeExpander[Tree](NEW_ROLE, typer, original, tpt, targs, argss1) {
      // TODO: make type macros expanding in new role to behave template-like
      // it kind of makes sense to e.g. let `new TM` expand into `new C { def x = 2 }`
      override def isParentLike = true
      override def prepare(desugared: Tree) = typer.typed(desugared, EXPRmode, WildcardType)
      override def onSuccess(expanded: Tree) = typer.typed(repackApplyAsNew(expanded), mode, WildcardType)
      override def allowResult(result: Tree) = allowResultTree(result)
    }
    expander(original)
  }

  /** Expands a macro type used in annotation role as `TM(2)(3)` in `@TM(2)(3) class C`.
   *  @see MacroExpander
   */
  def macroExpandAnnotation(typer: Typer, original: Tree, tpt: Tree, targs: List[Tree], argss: List[List[Tree]], mode: Mode, selfsym: Symbol) = {
    // TODO: see an explanation of the situation in comments to `macroExpandNew`
    val argss1 = if (argss == ListOfNil) Nil else argss
    object expander extends MacroTypeExpander[AnnotationInfo](ANNOTATION_ROLE, typer, original, tpt, targs, argss1) {
      override def isParentLike = true
      override def prepare(desugared: Tree) = typer.typed(desugared, EXPRmode, WildcardType)
      override def onSuccess(expanded: Tree) = typer.typedAnnotation(repackApplyAsNew(expanded), mode, selfsym)
      override def onFailure(expanded: Tree) = { typer.infer.setError(original); ErroneousAnnotation }
      override def allowResult(result: AnnotationInfo) = allowResultAnn(result)
    }
    expander(original)
  }

  /** Captures statuses of macro expansions performed by `macroExpand1'.
   */
  private sealed abstract class MacroStatus(val result: Tree)
  private case class Success(expanded: Tree) extends MacroStatus(expanded)
  private case class Fallback(fallback: Tree) extends MacroStatus(fallback) { currentRun.seenMacroExpansionsFallingBack = true }
  private case class Delayed(delayed: Tree) extends MacroStatus(delayed)
  private case class Skipped(skipped: Tree) extends MacroStatus(skipped)
  private case class Failure(failure: Tree) extends MacroStatus(failure)
  private def Delay(expanded: Tree) = Delayed(expanded)
  private def Skip(expanded: Tree) = Skipped(expanded)
  private def Cancel(expandee: Tree) = Failure(expandee)

  /** Does the same as `macroExpand`, but without typechecking the expansion
   *  Meant for internal use within the macro infrastructure, don't use it elsewhere.
   */
  private def macroExpand1(typer: Typer, expandee: Tree): MacroStatus = {
    // verbose printing might cause recursive macro expansions, so I'm shutting it down here
    withInfoLevel(nodePrinters.InfoLevel.Quiet) {
      if (expandee.symbol.isErroneous || (expandee exists (_.isErroneous))) {
        val reason = if (expandee.symbol.isErroneous) "not found or incompatible macro implementation" else "erroneous arguments"
        macroTraceVerbose("cancelled macro expansion because of %s: ".format(reason))(expandee)
        Cancel(typer.infer.setError(expandee))
      }
      else try {
        val runtime = macroRuntime(expandee.symbol, FLAVOR_EXPAND)
        if (runtime != null) macroExpandWithRuntime(typer, expandee, runtime)
        else macroExpandWithoutRuntime(typer, expandee)
      } catch {
        case typer.TyperErrorGen.MacroExpansionException => Failure(expandee)
      }
    }
  }

  /** Expands a macro when a runtime (i.e. the macro implementation) can be successfully loaded
   *  Meant for internal use within the macro infrastructure, don't use it elsewhere.
   */
  private def macroExpandWithRuntime(typer: Typer, expandee: Tree, runtime: MacroRuntime): MacroStatus = {
    val wasDelayed  = isDelayed(expandee)
    val undetparams = calculateUndetparams(expandee)
    val nowDelayed  = !typer.context.macrosEnabled || undetparams.nonEmpty

    (wasDelayed, nowDelayed) match {
      case (true, true) =>
        Delay(expandee)
      case (true, false) =>
        val expanded = macroExpandAll(typer, expandee)
        if (expanded exists (_.isErroneous)) Failure(expandee)
        else Skip(expanded)
      case (false, true) =>
        macroLogLite("macro expansion is delayed: %s".format(expandee))
        delayed += expandee -> undetparams
        expandee updateAttachment MacroRuntimeAttachment(delayed = true, typerContext = typer.context, macroContext = Some(macroArgs(typer, expandee).c))
        Delay(expandee)
      case (false, false) =>
        import typer.TyperErrorGen._
        macroLogLite("performing macro expansion %s at %s".format(expandee, expandee.pos))
        val args = macroArgs(typer, expandee)
        try {
          val numErrors    = reporter.ERROR.count
          def hasNewErrors = reporter.ERROR.count > numErrors
          val expanded = { pushMacroContext(args.c); runtime(args) }
          if (hasNewErrors) MacroGeneratedTypeError(expandee)
          def validateResultingTree(expanded: Tree) = {
            macroLogVerbose("original:")
            macroLogLite("" + expanded + "\n" + showRaw(expanded))
            val freeSyms = expanded.freeTerms ++ expanded.freeTypes
            freeSyms foreach (sym => MacroFreeSymbolError(expandee, sym))
            Success(atPos(enclosingMacroPosition.focus)(expanded))
          }
          expanded match {
            case expanded: Expr[_] => validateResultingTree(expanded.tree)
            case expanded: Tree => validateResultingTree(expanded)
            case _ => MacroExpansionHasInvalidTypeError(expandee, expanded)
          }
        } catch {
          case ex: Throwable =>
            popMacroContext()
            val realex = ReflectionUtils.unwrapThrowable(ex)
            realex match {
              case ex: AbortMacroException => MacroGeneratedAbort(expandee, ex)
              case ex: ControlThrowable => throw ex
              case ex: TypeError => MacroGeneratedTypeError(expandee, ex)
              case _ => MacroGeneratedException(expandee, realex)
            }
        } finally {
          expandee.removeAttachment[MacroRuntimeAttachment]
        }
    }
  }

  /** Expands a macro when a runtime (i.e. the macro implementation) cannot be loaded
   *  Meant for internal use within the macro infrastructure, don't use it elsewhere.
   */
  private def macroExpandWithoutRuntime(typer: Typer, expandee: Tree): MacroStatus = {
    import typer.TyperErrorGen._
    val fallbackSym = expandee.symbol.nextOverriddenSymbol orElse MacroImplementationNotFoundError(expandee)
    macroTraceLite("falling back to: ")(fallbackSym)

    def mkFallbackTree(tree: Tree): Tree = {
      tree match {
        case Select(qual, name) => Select(qual, name) setPos tree.pos setSymbol fallbackSym
        case Apply(fn, args) => Apply(mkFallbackTree(fn), args) setPos tree.pos
        case TypeApply(fn, args) => TypeApply(mkFallbackTree(fn), args) setPos tree.pos
      }
    }
    Fallback(mkFallbackTree(expandee))
  }

  /** Without any restrictions on macro expansion, macro applications will expand at will,
   *  and when type inference is involved, expansions will end up using yet uninferred type params.
   *
   *  For some macros this might be ok (thanks to TreeTypeSubstituter that replaces
   *  the occurrences of undetparams with their inferred values), but in general case this won't work.
   *  E.g. for reification simple substitution is not enough - we actually need to re-reify inferred types.
   *
   *  Luckily, there exists a very simple way to fix the problem: delay macro expansion until everything is inferred.
   *  Here are the exact rules. Macro application gets delayed if any of its subtrees contain:
   *    1) type vars (tpe.isInstanceOf[TypeVar]) // [Eugene] this check is disabled right now, because TypeVars seem to be created from undetparams anyways
   *    2) undetparams (sym.isTypeParameter && !sym.isSkolem)
   */
  var hasPendingMacroExpansions = false
  private val delayed = perRunCaches.newWeakMap[Tree, scala.collection.mutable.Set[Int]]
  private def isDelayed(expandee: Tree) = delayed contains expandee
  private def calculateUndetparams(expandee: Tree): scala.collection.mutable.Set[Int] =
    delayed.get(expandee).getOrElse {
      val calculated = scala.collection.mutable.Set[Symbol]()
      val treeInfo.Applied(core, _, argss) = expandee
      val traversalRoots = if (isUntypedMacroApplication(core)) argss.flatten else List(expandee)
      traversalRoots foreach (_ foreach (sub => {
        def traverse(sym: Symbol) = if (sym != null && (undetparams contains sym.id)) calculated += sym
        if (sub.symbol != null) traverse(sub.symbol)
        if (sub.tpe != null) sub.tpe foreach (sub => traverse(sub.typeSymbol))
      }))
      macroLogVerbose("calculateUndetparams: %s".format(calculated))
      calculated map (_.id)
    }
  private val undetparams = perRunCaches.newSet[Int]
  def notifyUndetparamsAdded(newUndets: List[Symbol]): Unit = {
    undetparams ++= newUndets map (_.id)
    if (macroDebugVerbose) newUndets foreach (sym => println("undetParam added: %s".format(sym)))
  }
  def notifyUndetparamsInferred(undetNoMore: List[Symbol], inferreds: List[Type]): Unit = {
    undetparams --= undetNoMore map (_.id)
    if (macroDebugVerbose) (undetNoMore zip inferreds) foreach { case (sym, tpe) => println("undetParam inferred: %s as %s".format(sym, tpe))}
    if (!delayed.isEmpty)
      delayed.toList foreach {
        case (expandee, undetparams) if !undetparams.isEmpty =>
          undetparams --= undetNoMore map (_.id)
          if (undetparams.isEmpty) {
            hasPendingMacroExpansions = true
            macroTraceVerbose("macro expansion is pending: ")(expandee)
          }
        case _ =>
          // do nothing
      }
  }

  /** Performs macro expansion on all subtrees of a given tree.
   *  Innermost macros are expanded first, outermost macros are expanded last.
   *  See the documentation for `macroExpand` for more information.
   */
  def macroExpandAll(typer: Typer, expandee: Tree): Tree =
    new Transformer {
      override def transform(tree: Tree) = super.transform(tree match {
        // todo. expansion should work from the inside out
        case tree if (delayed contains tree) && calculateUndetparams(tree).isEmpty && !tree.isErroneous =>
          val context = tree.attachments.get[MacroRuntimeAttachment].get.typerContext
          delayed -= tree
          context.implicitsEnabled = typer.context.implicitsEnabled
          context.enrichmentEnabled = typer.context.enrichmentEnabled
          context.macrosEnabled = typer.context.macrosEnabled
          // if it's a type macro, its expansion will be typechecked elsewhere
          // if it's a term macro, we need to process the expansion immediately
          if (tree.symbol.isTypeMacro) {
            assert(tree == expandee, s"tree = ${showRaw(tree)}, expandee = ${showRaw(expandee)}")
            macroExpand1(newTyper(context), tree).result
          } else {
            macroExpandApply(newTyper(context), tree, EXPRmode, WildcardType)
          }
        case _ =>
          tree
      })
    }.transform(expandee)
}

object MacrosStats {
  import scala.reflect.internal.TypesStats.typerNanos
  val macroExpandCount    = Statistics.newCounter ("#macro expansions", "typer")
  val macroExpandNanos    = Statistics.newSubTimer("time spent in macroExpand", typerNanos)
}
