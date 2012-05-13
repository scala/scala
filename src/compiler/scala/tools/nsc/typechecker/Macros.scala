package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.tools.nsc.util._
import scala.tools.nsc.util.ClassPath._
import scala.reflect.ReflectionUtils
import scala.collection.mutable.ListBuffer
import scala.compat.Platform.EOL
import scala.reflect.makro.runtime.{Context => MacroContext}
import scala.reflect.runtime.Mirror
import util.Statistics._
import scala.reflect.makro.util._

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
 *    def fooBar[T: c.TypeTag]
 *           (c: scala.reflect.makro.Context)
 *           (xs: c.Expr[List[T]])
 *           : c.mirror.Tree = {
 *      ...
 *    }
 *
 *  Then, if foo is called in qual.foo[Int](elems), where qual: D,
 *  the macro application is expanded to a reflective invocation of fooBar with parameters
 *
 *    (simpleMacroContext{ type PrefixType = D; val prefix = qual })
 *    (Expr(elems))
 *    (TypeTag(Int))
 */
trait Macros extends Traces {
  self: Analyzer =>

  import global._
  import definitions._
  def globalSettings = global.settings

  val globalMacroCache = collection.mutable.Map[Any, Any]()
  val perRunMacroCache = perRunCaches.newMap[Symbol, collection.mutable.Map[Any, Any]]

  /** A list of compatible macro implementation signatures.
   *
   *  In the example above:
   *    (c: scala.reflect.makro.Context)(xs: c.Expr[List[T]]): c.Expr[T]
   *
   *  @param macroDef The macro definition symbol
   *  @param tparams  The type parameters of the macro definition
   *  @param vparamss The value parameters of the macro definition
   *  @param retTpe   The return type of the macro definition
   */
  private def macroImplSigs(macroDef: Symbol, tparams: List[TypeDef], vparamss: List[List[ValDef]], retTpe: Type): (List[List[List[Symbol]]], Type) = {
    // had to move method's body to an object because of the recursive dependencies between sigma and param
    object SigGenerator {
      val hasThis = macroDef.owner.isClass
      val ownerTpe = macroDef.owner match {
        case owner if owner.isModuleClass => new UniqueThisType(macroDef.owner)
        case owner if owner.isClass => macroDef.owner.tpe
        case _ => NoType
      }
      val hasTparams = !tparams.isEmpty

      def sigma(tpe: Type): Type = {
        class SigmaTypeMap extends TypeMap {
          def apply(tp: Type): Type = tp match {
            case TypeRef(pre, sym, args) =>
              val pre1 = pre match {
                case ThisType(sym) if sym == macroDef.owner =>
                  SingleType(SingleType(SingleType(NoPrefix, paramsCtx(0)), MacroContextPrefix), ExprValue)
                case SingleType(NoPrefix, sym) =>
                  mfind(vparamss)(_.symbol == sym) match {
                    case Some(macroDefParam) =>
                      SingleType(SingleType(NoPrefix, param(macroDefParam)), ExprValue)
                    case _ =>
                      pre
                  }
                case _ =>
                  pre
              }
              val args1 = args map mapOver
              TypeRef(pre1, sym, args1)
            case _ =>
              mapOver(tp)
          }
        }

        new SigmaTypeMap() apply tpe
      }

      def makeParam(name: Name, pos: Position, tpe: Type, flags: Long = 0L) =
        macroDef.newValueParameter(name, pos, flags) setInfo tpe
      val ctxParam = makeParam(nme.macroContext, macroDef.pos, MacroContextClass.tpe, SYNTHETIC)
      def implType(isType: Boolean, origTpe: Type): Type =
        if (isRepeatedParamType(origTpe))
          appliedType(
            RepeatedParamClass.typeConstructor,
            List(implType(isType, sigma(origTpe.typeArgs.head))))
        else {
          val tsym = getMember(MacroContextClass, if (isType) tpnme.TypeTag else tpnme.Expr)
          typeRef(singleType(NoPrefix, ctxParam), tsym, List(sigma(origTpe)))
        }
      val paramCache = collection.mutable.Map[Symbol, Symbol]()
      def param(tree: Tree): Symbol =
        paramCache.getOrElseUpdate(tree.symbol, {
          // [Eugene] deskolemization became necessary once I implemented inference of macro def return type
          // please, verify this solution, but for now I'll leave it here - cargo cult for the win
          val sym = tree.symbol.deSkolemize
          val sigParam = makeParam(sym.name, sym.pos, implType(sym.isType, sym.tpe))
          if (sym.isSynthetic) sigParam.flags |= SYNTHETIC
          sigParam
        })

      val paramsCtx = List(ctxParam)
      val paramsThis = List(makeParam(nme.macroThis, macroDef.pos, implType(false, ownerTpe), SYNTHETIC))
      val paramsTparams = tparams map param
      val paramssParams = mmap(vparamss)(param)

      var paramsss = List[List[List[Symbol]]]()
      // tparams are no longer part of a signature, they get into macro implementations via context bounds
//      if (hasTparams && hasThis) paramsss :+= paramsCtx :: paramsThis :: paramsTparams :: paramssParams
//      if (hasTparams) paramsss :+= paramsCtx :: paramsTparams :: paramssParams
      // _this params are no longer part of a signature, its gets into macro implementations via Context.prefix
//      if (hasThis) paramsss :+= paramsCtx :: paramsThis :: paramssParams
      paramsss :+= paramsCtx :: paramssParams

      val tsym = getMember(MacroContextClass, tpnme.Expr)
      val implRetTpe = typeRef(singleType(NoPrefix, ctxParam), tsym, List(sigma(retTpe)))
    }

    import SigGenerator._
    macroTraceVerbose("generating macroImplSigs for: ")(macroDef)
    macroTraceVerbose("tparams are: ")(tparams)
    macroTraceVerbose("vparamss are: ")(vparamss)
    macroTraceVerbose("retTpe is: ")(retTpe)
    macroTraceVerbose("macroImplSigs are: ")(paramsss, implRetTpe)
  }

  private def transformTypeTagEvidenceParams(paramss: List[List[Symbol]], transform: (Symbol, Symbol) => Option[Symbol]): List[List[Symbol]] = {
    if (paramss.length == 0)
      return paramss

    val wannabe = if (paramss.head.length == 1) paramss.head.head else NoSymbol
    val contextParam = if (wannabe != NoSymbol && wannabe.tpe <:< definitions.MacroContextClass.tpe) wannabe else NoSymbol

    val lastParamList0 = paramss.lastOption getOrElse Nil
    val lastParamList = lastParamList0 flatMap (param => param.tpe match {
      case TypeRef(SingleType(NoPrefix, contextParam), sym, List(tparam)) =>
        var wannabe = sym
        while (wannabe.isAliasType) wannabe = wannabe.info.typeSymbol
        if (wannabe != definitions.TypeTagClass && wannabe != definitions.ConcreteTypeTagClass)
          List(param)
        else
          transform(param, tparam.typeSymbol) map (_ :: Nil) getOrElse Nil
      case _ =>
        List(param)
    })

    var result = paramss.dropRight(1) :+ lastParamList
    if (lastParamList0.isEmpty ^ lastParamList.isEmpty) {
      result = result dropRight 1
    }

    result
  }

  /** As specified above, body of a macro definition must reference its implementation.
   *  This function verifies that the body indeed refers to a method, and that
   *  the referenced macro implementation is compatible with the given macro definition.
   *
   *  This means that macro implementation (fooBar in example above) must:
   *    1) Refer to a statically accessible, non-overloaded method.
   *    2) Have the right parameter lists as outlined in the SIP / in the doc comment of this class.
   *
   *  @return typechecked rhs of the given macro definition
   */
  def typedMacroBody(typer: Typer, ddef: DefDef): Tree = {
    import typer.context
    macroLogVerbose("typechecking macro def %s at %s".format(ddef.symbol, ddef.pos))

    if (!typer.checkFeature(ddef.pos, MacrosFeature, immediate = true)) {
      ddef.symbol setFlag IS_ERROR
      return EmptyTree
    }

    implicit class AugmentedString(s: String) {
      def abbreviateCoreAliases: String = { // hack!
        var result = s
        result = result.replace("c.mirror.TypeTag", "c.TypeTag")
        result = result.replace("c.mirror.Expr", "c.Expr")
        result
      }
    }

    var hasErrors = false
    def reportError(pos: Position, msg: String) = {
      hasErrors = true
      context.error(pos, msg)
    }

    val macroDef = ddef.symbol
    val defpos = macroDef.pos
    val implpos = ddef.rhs.pos
    assert(macroDef.isTermMacro, ddef)

    def invalidBodyError() =
      reportError(defpos,
        "macro body has wrong shape:" +
        "\n required: macro <reference to implementation object>.<implementation method name>" +
        "\n or      : macro <implementation method name>")
    def validatePreTyper(rhs: Tree): Unit = rhs match {
      // we do allow macro invocations inside macro bodies
      // personally I don't mind if pre-typer tree is a macro invocation
      // that later resolves to a valid reference to a macro implementation
      // however, I don't think that invalidBodyError() should hint at that
      // let this be an Easter Egg :)
      case Apply(_, _) => ;
      case TypeApply(_, _) => ;
      case Super(_, _) => ;
      case This(_) => ;
      case Ident(_) => ;
      case Select(_, _) => ;
      case _ => invalidBodyError()
    }
    def validatePostTyper(rhs1: Tree): Unit = {
      def loop(tree: Tree): Unit = {
        def errorNotStatic() =
          reportError(implpos, "macro implementation must be in statically accessible object")

        def ensureRoot(sym: Symbol) =
          if (!sym.isModule && !sym.isModuleClass) errorNotStatic()

        def ensureModule(sym: Symbol) =
          if (!sym.isModule) errorNotStatic()

        tree match {
          case TypeApply(fun, _) =>
            loop(fun)
          case Super(qual, _) =>
            ensureRoot(macroDef.owner)
            loop(qual)
          case This(_) =>
            ensureRoot(tree.symbol)
          case Select(qual, name) if name.isTypeName =>
            loop(qual)
          case Select(qual, name) if name.isTermName =>
            if (tree.symbol != rhs1.symbol) ensureModule(tree.symbol)
            loop(qual)
          case Ident(name) if name.isTypeName =>
            ;
          case Ident(name) if name.isTermName =>
            if (tree.symbol != rhs1.symbol) ensureModule(tree.symbol)
          case _ =>
            invalidBodyError()
        }
      }

      loop(rhs1)
    }

    val rhs = ddef.rhs
    validatePreTyper(rhs)
    if (hasErrors) macroTraceVerbose("macro def failed to satisfy trivial preconditions: ")(macroDef)

    // we use typed1 instead of typed, because otherwise adapt is going to mess us up
    // if adapt sees <qualifier>.<method>, it will want to perform eta-expansion and will fail
    // unfortunately, this means that we have to manually trigger macro expansion
    // because it's adapt which is responsible for automatic expansion during typechecking
    def typecheckRhs(rhs: Tree): Tree = {
      try {
        val prevNumErrors = reporter.ERROR.count // [Eugene] funnily enough, the isErroneous check is not enough
        var rhs1 = if (hasErrors) EmptyTree else typer.typed1(rhs, EXPRmode, WildcardType)
        def typecheckedWithErrors = (rhs1 exists (_.isErroneous)) || reporter.ERROR.count != prevNumErrors
        def rhsNeedsMacroExpansion = rhs1.symbol != null && rhs1.symbol.isTermMacro && !rhs1.symbol.isErroneous
        while (!typecheckedWithErrors && rhsNeedsMacroExpansion) {
          rhs1 = macroExpand1(typer, rhs1) match {
            case Success(expanded) =>
              try {
                val typechecked = typer.typed1(expanded, EXPRmode, WildcardType)
                macroLogVerbose("typechecked1:%n%s%n%s".format(typechecked, showRaw(typechecked)))
                typechecked
              } finally {
                openMacros = openMacros.tail
              }
            case Fallback(fallback) =>
              typer.typed1(fallback, EXPRmode, WildcardType)
            case Other(result) =>
              result
          }
        }
        rhs1
      } catch {
        case ex: TypeError =>
          typer.reportTypeError(context, rhs.pos, ex)
          typer.infer.setError(rhs)
      }
    }

    val prevNumErrors = reporter.ERROR.count // funnily enough, the isErroneous check is not enough
    var rhs1 = typecheckRhs(rhs)
    def typecheckedWithErrors = (rhs1 exists (_.isErroneous)) || reporter.ERROR.count != prevNumErrors
    hasErrors = hasErrors || typecheckedWithErrors
    if (typecheckedWithErrors) macroTraceVerbose("body of a macro def failed to typecheck: ")(ddef)

    val macroImpl = rhs1.symbol
    macroDef withAnnotation AnnotationInfo(MacroImplAnnotation.tpe, List(rhs1), Nil)
    if (!hasErrors) {
      if (macroImpl == null) {
         invalidBodyError()
      } else {
        if (!macroImpl.isMethod)
           invalidBodyError()
        if (macroImpl.isOverloaded)
          reportError(implpos, "macro implementation cannot be overloaded")
        if (!macroImpl.typeParams.isEmpty && (!rhs1.isInstanceOf[TypeApply]))
          reportError(implpos, "macro implementation reference needs type arguments")
        if (!hasErrors)
          validatePostTyper(rhs1)
      }
      if (hasErrors)
        macroTraceVerbose("macro def failed to satisfy trivial preconditions: ")(macroDef)
    }

    if (!hasErrors) {
      def checkCompatibility(reqparamss: List[List[Symbol]], actparamss: List[List[Symbol]], reqres: Type, actres: Type): List[String] = {
        var hasErrors = false
        var errors = List[String]()
        def compatibilityError(msg: String) {
          hasErrors = true
          errors :+= msg
        }

        val flatreqparams = reqparamss.flatten
        val flatactparams = actparamss.flatten
        val tparams = macroImpl.typeParams
        val tvars = tparams map freshVar
        def lengthMsg(which: String, extra: Symbol) =
          "parameter lists have different length, "+which+" extra parameter "+extra.defString
        if (actparamss.length != reqparamss.length)
          compatibilityError("number of parameter sections differ")

        def checkSubType(slot: String, reqtpe: Type, acttpe: Type): Unit = {
          val ok = if (macroDebugVerbose) {
            if (reqtpe eq acttpe) println(reqtpe + " <: " + acttpe + "?" + EOL + "true")
            withTypesExplained(reqtpe <:< acttpe)
          } else reqtpe <:< acttpe
          if (!ok) {
            compatibilityError("type mismatch for %s: %s does not conform to %s".format(slot, reqtpe.toString.abbreviateCoreAliases, acttpe.toString.abbreviateCoreAliases))
          }
        }

        if (!hasErrors) {
          try {
            for ((rparams, aparams) <- reqparamss zip actparamss) {
              if (rparams.length < aparams.length)
                compatibilityError(lengthMsg("found", aparams(rparams.length)))
              if (aparams.length < rparams.length)
                compatibilityError(lengthMsg("required", rparams(aparams.length)).abbreviateCoreAliases)
            }
            // if the implementation signature is already deemed to be incompatible, we bail out
            // otherwise, high-order type magic employed below might crash in weird ways
            if (!hasErrors) {
              for ((rparams, aparams) <- reqparamss zip actparamss) {
                for ((rparam, aparam) <- rparams zip aparams) {
                  def isRepeated(param: Symbol) = param.tpe.typeSymbol == RepeatedParamClass
                  if (rparam.name != aparam.name && !rparam.isSynthetic) {
                    val rparam1 = rparam
                    val aparam1 = aparam
                    compatibilityError("parameter names differ: "+rparam.name+" != "+aparam.name)
                  }
                  if (isRepeated(rparam) && !isRepeated(aparam))
                    compatibilityError("types incompatible for parameter "+rparam.name+": corresponding is not a vararg parameter")
                  if (!isRepeated(rparam) && isRepeated(aparam))
                    compatibilityError("types incompatible for parameter "+aparam.name+": corresponding is not a vararg parameter")
                  if (!hasErrors) {
                    var atpe = aparam.tpe.substSym(flatactparams, flatreqparams).instantiateTypeParams(tparams, tvars)
                    atpe = atpe.dealias // SI-5706
                    // strip the { type PrefixType = ... } refinement off the Context or otherwise we get compatibility errors
                    atpe = atpe match {
                      case RefinedType(List(tpe), Scope(sym)) if tpe == MacroContextClass.tpe && sym.allOverriddenSymbols.contains(MacroContextPrefixType) => tpe
                      case _ => atpe
                    }
                    checkSubType("parameter " + rparam.name, rparam.tpe, atpe)
                  }
                }
              }
            }
            if (!hasErrors) {
              val atpe = actres.substSym(flatactparams, flatreqparams).instantiateTypeParams(tparams, tvars)
              checkSubType("return type", atpe, reqres)
            }
            if (!hasErrors) {
              val targs = solvedTypes(tvars, tparams, tparams map varianceInType(actres), false,
                lubDepth(flatactparams map (_.tpe)) max lubDepth(flatreqparams map (_.tpe)))
              val boundsOk = typer.silent(_.infer.checkBounds(ddef, NoPrefix, NoSymbol, tparams, targs, ""))
              boundsOk match {
                case SilentResultValue(true) => ;
                case SilentResultValue(false) | SilentTypeError(_) =>
                  val bounds = tparams map (tp => tp.info.instantiateTypeParams(tparams, targs).bounds)
                  compatibilityError("type arguments " + targs.mkString("[", ",", "]") +
                                     " do not conform to " + tparams.head.owner + "'s type parameter bounds " +
                                     (tparams map (_.defString)).mkString("[", ",", "]"))
              }
            }
          } catch {
            case ex: NoInstance =>
              compatibilityError(
                "type parameters "+(tparams map (_.defString) mkString ", ")+" cannot be instantiated\n"+
                ex.getMessage)
          }
        }

        errors.toList
      }

      var actparamss = macroImpl.paramss
      actparamss = transformTypeTagEvidenceParams(actparamss, (param, tparam) => None)

      val rettpe = if (!ddef.tpt.isEmpty) typer.typedType(ddef.tpt).tpe else computeMacroDefTypeFromMacroImpl(ddef, macroDef, macroImpl)
      val (reqparamsss0, reqres0) = macroImplSigs(macroDef, ddef.tparams, ddef.vparamss, rettpe)
      var reqparamsss = reqparamsss0

      // prohibit implicit params on macro implementations
      // we don't have to do this, but it appears to be more clear than allowing them
      val implicitParams = actparamss.flatten filter (_.isImplicit)
      if (implicitParams.length > 0) {
        reportError(implicitParams.head.pos, "macro implementations cannot have implicit parameters other than TypeTag evidences")
        macroTraceVerbose("macro def failed to satisfy trivial preconditions: ")(macroDef)
      }

      if (!hasErrors) {
        val reqres = reqres0
        val actres = macroImpl.tpe.finalResultType
        def showMeth(pss: List[List[Symbol]], restpe: Type, abbreviate: Boolean) = {
          var argsPart = (pss map (ps => ps map (_.defString) mkString ("(", ", ", ")"))).mkString
          if (abbreviate) argsPart = argsPart.abbreviateCoreAliases
          var retPart = restpe.toString
          if (abbreviate || ddef.tpt.tpe == null) retPart = retPart.abbreviateCoreAliases
          argsPart + ": " + retPart
        }
        def compatibilityError(addendum: String) =
          reportError(implpos,
            "macro implementation has wrong shape:"+
            "\n required: "+showMeth(reqparamsss.head, reqres, true) +
            (reqparamsss.tail map (paramss => "\n or      : "+showMeth(paramss, reqres, true)) mkString "")+
            "\n found   : "+showMeth(actparamss, actres, false)+
            "\n"+addendum)

        macroTraceVerbose("considering " + reqparamsss.length + " possibilities of compatible macro impl signatures for macro def: ")(ddef.name)
        val results = reqparamsss map (checkCompatibility(_, actparamss, reqres, actres))
        if (macroDebugVerbose) (reqparamsss zip results) foreach { case (reqparamss, result) =>
          println("%s %s".format(if (result.isEmpty) "[  OK  ]" else "[FAILED]", reqparamss))
          result foreach (errorMsg => println("  " + errorMsg))
        }

        if (results forall (!_.isEmpty)) {
          var index = reqparamsss indexWhere (_.length == actparamss.length)
          if (index == -1) index = 0
          val mostRelevantMessage = results(index).head
          compatibilityError(mostRelevantMessage)
        } else {
          assert((results filter (_.isEmpty)).length == 1, results)
          if (macroDebugVerbose) (reqparamsss zip results) filter (_._2.isEmpty) foreach { case (reqparamss, result) =>
            println("typechecked macro impl as: " + reqparamss)
          }
        }
      }
    }

    // if this macro definition is erroneous, then there's no sense in expanding its usages
    // in the previous prototype macro implementations were magically generated from macro definitions
    // so macro definitions and its usages couldn't be compiled in the same compilation run
    // however, now definitions and implementations are decoupled, so it's everything is possible
    // hence, we now use IS_ERROR flag to serve as an indicator that given macro definition is broken
    if (hasErrors) {
      macroDef setFlag IS_ERROR
    }

    rhs1
  }

  def computeMacroDefTypeFromMacroImpl(macroDdef: DefDef, macroDef: Symbol, macroImpl: Symbol): Type = {
    // get return type from method type
    def unwrapRet(tpe: Type): Type = {
      def loop(tpe: Type) = tpe match {
        case NullaryMethodType(ret) => ret
        case mtpe @ MethodType(_, ret) => unwrapRet(ret)
        case _ => tpe
      }

      tpe match {
        case PolyType(_, tpe) => loop(tpe)
        case _ => loop(tpe)
      }
    }
    var metaType = unwrapRet(macroImpl.tpe)

    // downgrade from metalevel-0 to metalevel-1
    def inferRuntimeType(metaType: Type): Type = metaType match {
      case TypeRef(pre, sym, args) if sym.name == tpnme.Expr && args.length == 1 =>
        args.head
      case _ =>
        AnyClass.tpe
    }
    var runtimeType = inferRuntimeType(metaType)

    // transform type parameters of a macro implementation into type parameters of a macro definition
    runtimeType = runtimeType map {
      case TypeRef(pre, sym, args) =>
        // [Eugene] not sure which of these deSkolemizes are necessary
        // sym.paramPos is unreliable (see another case below)
        val tparams = macroImpl.typeParams map (_.deSkolemize)
        val paramPos = tparams indexOf sym.deSkolemize
        val sym1 = if (paramPos == -1) sym else {
          val ann = macroDef.getAnnotation(MacroImplAnnotation)
          ann match {
            case Some(ann) =>
              val TypeApply(_, implRefTargs) = ann.args(0)
              val implRefTarg = implRefTargs(paramPos).tpe.typeSymbol
              implRefTarg
            case None =>
              sym
          }
        }
        TypeRef(pre, sym1, args)
      case tpe =>
        tpe
    }

    // as stated in the spec, before being matched to macroimpl, type and value parameters of macrodef
    // undergo a special transformation, sigma, that adapts them to the different metalevel macroimpl lives in
    // as a result, we need to reverse this transformation when inferring macrodef ret from macroimpl ret
    def unsigma(tpe: Type): Type = {
      // unfortunately, we cannot dereference ``paramss'', because we're in the middle of inferring a type for ``macroDef''
//      val defParamss = macroDef.paramss
      val defParamss = mmap(macroDdef.vparamss)(_.symbol)
      var implParamss = macroImpl.paramss
      implParamss = transformTypeTagEvidenceParams(implParamss, (param, tparam) => None)

      val implCtxParam = if (implParamss.length > 0 && implParamss(0).length > 0) implParamss(0)(0) else null
      def implParamToDefParam(implParam: Symbol): Symbol = {
        val indices = (((implParamss drop 1).zipWithIndex) map { case (implParams, index) => (index, implParams indexOf implParam) } filter (_._2 != -1)).headOption
        val defParam = indices flatMap {
          case (plistIndex, pIndex) =>
            if (defParamss.length <= plistIndex) None
            else if (defParamss(plistIndex).length <= pIndex) None
            else Some(defParamss(plistIndex)(pIndex))
        }
        defParam.orNull
      }

      class UnsigmaTypeMap extends TypeMap {
        def apply(tp: Type): Type = tp match {
          case TypeRef(pre, sym, args) =>
            val pre1 = pre match {
              case SingleType(SingleType(SingleType(NoPrefix, param), prefix), value) if param == implCtxParam && prefix == MacroContextPrefix && value == ExprValue =>
                ThisType(macroDef.owner)
              case SingleType(SingleType(NoPrefix, param), value) if implParamToDefParam(param) != null && value == ExprValue =>
                val macroDefParam = implParamToDefParam(param)
                SingleType(NoPrefix, macroDefParam)
              case _ =>
                pre
            }
            val args1 = args map mapOver
            TypeRef(pre1, sym, args1)
          case _ =>
            mapOver(tp)
        }
      }

      new UnsigmaTypeMap() apply tpe
    }
    runtimeType = unsigma(runtimeType)

    runtimeType
  }

  /** Primary mirror that is used to resolve and run macro implementations.
   *  Loads classes from -Xmacro-primary-classpath, or from -cp if the option is not specified.
   */
  private lazy val primaryMirror: Mirror = {
    if (global.forMSIL)
      throw new UnsupportedOperationException("Scala reflection not available on this platform")

    val libraryClassLoader = {
      if (settings.XmacroPrimaryClasspath.value != "") {
        macroLogVerbose("primary macro mirror: initializing from -Xmacro-primary-classpath: %s".format(settings.XmacroPrimaryClasspath.value))
        val classpath = toURLs(settings.XmacroFallbackClasspath.value)
        ScalaClassLoader.fromURLs(classpath, self.getClass.getClassLoader)
      } else {
        macroLogVerbose("primary macro mirror: initializing from -cp: %s".format(global.classPath.asURLs))
        val classpath = global.classPath.asURLs
        var loader: ClassLoader = ScalaClassLoader.fromURLs(classpath, self.getClass.getClassLoader)

        // [Eugene] a heuristic to detect REPL
        if (global.settings.exposeEmptyPackage.value) {
          import scala.tools.nsc.interpreter._
          val virtualDirectory = global.settings.outputDirs.getSingleOutput.get
          loader = new AbstractFileClassLoader(virtualDirectory, loader) {}
        }

        loader
      }
    }

    new Mirror(libraryClassLoader) { override def toString = "<primary macro mirror>" }
  }

  /** Fallback mirror that is used to resolve and run macro implementations.
   *  Loads classes from -Xmacro-fallback-classpath aka "macro fallback classpath".
   */
  private lazy val fallbackMirror: Mirror = {
    if (global.forMSIL)
      throw new UnsupportedOperationException("Scala reflection not available on this platform")

    val fallbackClassLoader = {
      macroLogVerbose("fallback macro mirror: initializing from -Xmacro-fallback-classpath: %s".format(settings.XmacroFallbackClasspath.value))
      val classpath = toURLs(settings.XmacroFallbackClasspath.value)
      ScalaClassLoader.fromURLs(classpath, self.getClass.getClassLoader)
    }

    new Mirror(fallbackClassLoader) { override def toString = "<fallback macro mirror>" }
  }

  /** Produces a function that can be used to invoke macro implementation for a given macro definition:
   *    1) Looks up macro implementation symbol in this universe.
   *    2) Loads its enclosing class from the primary mirror.
   *    3) Loads the companion of that enclosing class from the primary mirror.
   *    4) Resolves macro implementation within the loaded companion.
   *    5) If 2-4 fails, repeats them for the fallback mirror.
   *
   *  @return Some(runtime) if macro implementation can be loaded successfully from either of the mirrors,
   *          None otherwise.
   */
  private type MacroRuntime = List[Any] => Any
  private val macroRuntimesCache = perRunCaches.newWeakMap[Symbol, Option[MacroRuntime]]
  private lazy val fastTrack: Map[Symbol, MacroRuntime] = {
    import scala.reflect.api.Universe
    import scala.reflect.makro.internal._
    Map( // challenge: how can we factor out the common code? Does not seem to be easy.
      MacroInternal_materializeArrayTag -> (args => {
        assert(args.length == 3, args)
        val c = args(0).asInstanceOf[MacroContext]
        materializeArrayTag_impl(c)(args(1).asInstanceOf[c.Expr[Universe]])(args(2).asInstanceOf[c.TypeTag[_]])
      }),
      MacroInternal_materializeErasureTag -> (args => {
        assert(args.length == 3, args)
        val c = args(0).asInstanceOf[MacroContext]
        materializeErasureTag_impl(c)(args(1).asInstanceOf[c.Expr[Universe]])(args(2).asInstanceOf[c.TypeTag[_]])
      }),
      MacroInternal_materializeClassTag -> (args => {
        assert(args.length == 3, args)
        val c = args(0).asInstanceOf[MacroContext]
        materializeClassTag_impl(c)(args(1).asInstanceOf[c.Expr[Universe]])(args(2).asInstanceOf[c.TypeTag[_]])
      }),
      MacroInternal_materializeTypeTag -> (args => {
        assert(args.length == 3, args)
        val c = args(0).asInstanceOf[MacroContext]
        materializeTypeTag_impl(c)(args(1).asInstanceOf[c.Expr[Universe]])(args(2).asInstanceOf[c.TypeTag[_]])
      }),
      MacroInternal_materializeConcreteTypeTag -> (args => {
        assert(args.length == 3, args)
        val c = args(0).asInstanceOf[MacroContext]
        materializeConcreteTypeTag_impl(c)(args(1).asInstanceOf[c.Expr[Universe]])(args(2).asInstanceOf[c.TypeTag[_]])
      })
    )
  }
  private def macroRuntime(macroDef: Symbol): Option[MacroRuntime] = {
    macroTraceVerbose("looking for macro implementation: ")(macroDef)
    if (fastTrack contains macroDef) {
      macroLogVerbose("macro expansion serviced by a fast track")
      Some(fastTrack(macroDef))
    } else {
      macroRuntimesCache.getOrElseUpdate(macroDef, {
        val runtime = {
          macroTraceVerbose("looking for macro implementation: ")(macroDef)
          macroTraceVerbose("macroDef is annotated with: ")(macroDef.annotations)

          val ann = macroDef.getAnnotation(MacroImplAnnotation)
          if (ann == None) {
            macroTraceVerbose("@macroImpl annotation is missing (this means that macro definition failed to typecheck)")(macroDef)
            return None
          }

          val macroImpl = ann.get.args(0).symbol
          if (macroImpl == NoSymbol) {
            macroTraceVerbose("@macroImpl annotation is malformed (this means that macro definition failed to typecheck)")(macroDef)
            return None
          }

          macroLogVerbose("resolved implementation %s at %s".format(macroImpl, macroImpl.pos))
          if (macroImpl.isErroneous) {
            macroTraceVerbose("macro implementation is erroneous (this means that either macro body or macro implementation signature failed to typecheck)")(macroDef)
            return None
          }

          def loadMacroImpl(macroMirror: Mirror): Option[(Object, macroMirror.Symbol)] = {
            try {
              // this logic relies on the assumptions that were valid for the old macro prototype
              // namely that macro implementations can only be defined in top-level classes and modules
              // with the new prototype that materialized in a SIP, macros need to be statically accessible, which is different
              // for example, a macro def could be defined in a trait that is implemented by an object
              // there are some more clever cases when seemingly non-static method ends up being statically accessible
              // however, the code below doesn't account for these guys, because it'd take a look of time to get it right
              // for now I leave it as a todo and move along to more the important stuff

              macroTraceVerbose("loading implementation class from %s: ".format(macroMirror))(macroImpl.owner.fullName)
              macroTraceVerbose("classloader is: ")("%s of type %s".format(macroMirror.classLoader, if (macroMirror.classLoader != null) macroMirror.classLoader.getClass.toString else "primordial classloader"))
              def inferClasspath(cl: ClassLoader) = cl match {
                case cl: java.net.URLClassLoader => "[" + (cl.getURLs mkString ",") + "]"
                case null => "[" + scala.tools.util.PathResolver.Environment.javaBootClassPath + "]"
                case _ => "<unknown>"
              }
              macroTraceVerbose("classpath is: ")(inferClasspath(macroMirror.classLoader))

              // [Eugene] relies on the fact that macro implementations can only be defined in static classes
              // [Martin to Eugene] There's similar logic buried in Symbol#flatname. Maybe we can refactor?
              def classfile(sym: Symbol): String = {
                def recur(sym: Symbol): String = sym match {
                  case sym if sym.owner.isPackageClass =>
                    val suffix = if (sym.isModuleClass) "$" else ""
                    sym.fullName + suffix
                  case sym =>
                    val separator = if (sym.owner.isModuleClass) "" else "$"
                    recur(sym.owner) + separator + sym.javaSimpleName.toString
                }

                if (sym.isClass || sym.isModule) recur(sym)
                else recur(sym.enclClass)
              }

              // [Eugene] this doesn't work for inner classes
              // neither does macroImpl.owner.javaClassName, so I had to roll my own implementation
              //val receiverName = macroImpl.owner.fullName
              val implClassName = classfile(macroImpl.owner)
              val implClassSymbol: macroMirror.Symbol = macroMirror.symbolForName(implClassName)

              if (macroDebugVerbose) {
                println("implClassSymbol is: " + implClassSymbol.fullNameString)

                if (implClassSymbol != macroMirror.NoSymbol) {
                  val implClass = macroMirror.classToJava(implClassSymbol)
                  val implSource = implClass.getProtectionDomain.getCodeSource
                  println("implClass is %s from %s".format(implClass, implSource))
                  println("implClassLoader is %s with classpath %s".format(implClass.getClassLoader, inferClasspath(implClass.getClassLoader)))
                }
              }

              val implObjSymbol = implClassSymbol.companionModule
              macroTraceVerbose("implObjSymbol is: ")(implObjSymbol.fullNameString)

              if (implObjSymbol == macroMirror.NoSymbol) None
              else {
                // yet another reflection method that doesn't work for inner classes
                //val receiver = macroMirror.companionInstance(receiverClass)
                val implObj = try {
                  val implObjClass = java.lang.Class.forName(implClassName, true, macroMirror.classLoader)
                  implObjClass getField "MODULE$" get null
                } catch {
                  case ex: NoSuchFieldException => macroTraceVerbose("exception when loading implObj: ")(ex); null
                  case ex: NoClassDefFoundError => macroTraceVerbose("exception when loading implObj: ")(ex); null
                  case ex: ClassNotFoundException => macroTraceVerbose("exception when loading implObj: ")(ex); null
                }

                if (implObj == null) None
                else {
                  val implMethSymbol = implObjSymbol.info.member(macroMirror.newTermName(macroImpl.name.toString))
                  macroLogVerbose("implMethSymbol is: " + implMethSymbol.fullNameString)
                  macroLogVerbose("jimplMethSymbol is: " + macroMirror.methodToJava(implMethSymbol))

                  if (implMethSymbol == macroMirror.NoSymbol) None
                  else {
                    macroLogVerbose("successfully loaded macro impl as (%s, %s)".format(implObj, implMethSymbol))
                    Some((implObj, implMethSymbol))
                  }
                }
              }
            } catch {
              case ex: ClassNotFoundException =>
                macroTraceVerbose("implementation class failed to load: ")(ex.toString)
                None
            }
          }

          val primary = loadMacroImpl(primaryMirror)
          primary match {
            case Some((implObj, implMethSymbol)) =>
              def runtime(args: List[Any]) = primaryMirror.invoke(implObj, implMethSymbol)(args: _*).asInstanceOf[Any]
              Some(runtime _)
            case None =>
              if (settings.XmacroFallbackClasspath.value != "") {
                macroLogVerbose("trying to load macro implementation from the fallback mirror: %s".format(settings.XmacroFallbackClasspath.value))
                val fallback = loadMacroImpl(fallbackMirror)
                fallback match {
                  case Some((implObj, implMethSymbol)) =>
                    def runtime(args: List[Any]) = fallbackMirror.invoke(implObj, implMethSymbol)(args: _*).asInstanceOf[Any]
                    Some(runtime _)
                  case None =>
                    None
                }
              } else {
                None
              }
          }
        }

        if (runtime == None) macroDef setFlag IS_ERROR
        runtime
      })
    }
  }

  /** Should become private again once we're done with migrating typetag generation from implicits */
  def macroContext(typer: Typer, prefixTree: Tree, expandeeTree: Tree): MacroContext { val mirror: global.type } =
    new {
      val mirror: global.type = global
      val callsiteTyper: mirror.analyzer.Typer = typer.asInstanceOf[global.analyzer.Typer]
      // todo. infer precise typetag for this Expr, namely the PrefixType member of the Context refinement
      val prefix = Expr(prefixTree)(TypeTag.Nothing)
      val expandee = expandeeTree
    } with MacroContext {
      override def toString = "MacroContext(%s@%s +%d)".format(expandee.symbol.name, expandee.pos, enclosingMacros.length - 1 /* exclude myself */)
    }

  /** Calculate the arguments to pass to a macro implementation when expanding the provided tree.
   *
   *  This includes inferring the exact type and instance of the macro context to pass, and also
   *  allowing for missing parameter sections in macro implementation (see ``macroImplParamsss'' for more info).
   *
   *  @return list of runtime objects to pass to the implementation obtained by ``macroRuntime''
   */
  private def macroArgs(typer: Typer, expandee: Tree): Option[List[Any]] = {
    val macroDef         = expandee.symbol
    val runtime          = macroRuntime(macroDef) orElse { return None }
    var prefixTree: Tree = EmptyTree
    var typeArgs         = List[Tree]()
    val exprArgs         = ListBuffer[List[Expr[_]]]()

    def collectMacroArgs(tree: Tree): Unit = tree match {
      case Apply(fn, args) =>
        // todo. infer precise typetag for this Expr, namely the declared type of the corresponding macro impl argument
        exprArgs.prepend(args map (Expr(_)(TypeTag.Nothing)))
        collectMacroArgs(fn)
      case TypeApply(fn, args) =>
        typeArgs = args
        collectMacroArgs(fn)
      case Select(qual, name) =>
        prefixTree = qual
      case _ =>
    }
    collectMacroArgs(expandee)
    val context = expandee.attachmentOpt[MacroAttachment].flatMap(_.macroContext).getOrElse(macroContext(typer, prefixTree, expandee))
    var argss: List[List[Any]] = List(context) :: exprArgs.toList
    macroTraceVerbose("argss: ")(argss)

    val ann = macroDef.getAnnotation(MacroImplAnnotation).getOrElse(throw new Error("assertion failed. %s: %s".format(macroDef, macroDef.annotations)))
    val macroImpl = ann.args(0).symbol
    var paramss = macroImpl.paramss
    val tparams = macroImpl.typeParams
    macroTraceVerbose("paramss: ")(paramss)

    // we need to take care of all possible combos of nullary/empty-paramlist macro defs vs nullary/empty-arglist invocations
    // nullary def + nullary invocation => paramss and argss match, everything is okay
    // nullary def + empty-arglist invocation => illegal Scala code, impossible, everything is okay
    // empty-paramlist def + nullary invocation => uh-oh, we need to append a List() to argss
    // empty-paramlist def + empty-arglist invocation => paramss and argss match, everything is okay
    // that's almost it, but we need to account for the fact that paramss might have context bounds that mask the empty last paramlist
    val paramss_without_evidences = transformTypeTagEvidenceParams(paramss, (param, tparam) => None)
    val isEmptyParamlistDef = paramss_without_evidences.nonEmpty && paramss_without_evidences.last.isEmpty
    val isEmptyArglistInvocation = argss.nonEmpty && argss.last.isEmpty
    if (isEmptyParamlistDef && !isEmptyArglistInvocation) {
      macroLogVerbose("isEmptyParamlistDef && !isEmptyArglistInvocation: appending a List() to argss")
      argss = argss :+ Nil
    }

    // nb! check partial application against paramss without evidences
    val numParamLists = paramss_without_evidences.length
    val numArgLists = argss.length
    if (numParamLists != numArgLists) {
      typer.TyperErrorGen.MacroPartialApplicationError(expandee)
      return None
    }

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
    // then T and U need to be inferred from the lexical scope of the call using ``asSeenFrom''
    // whereas V won't be resolved by asSeenFrom and need to be loaded directly from ``expandee'' which needs to contain a TypeApply node
    // also, macro implementation reference may contain a regular type as a type argument, then we pass it verbatim
    val resolved = collection.mutable.Map[Symbol, Type]()
    paramss = transformTypeTagEvidenceParams(paramss, (param, tparam) => {
      val TypeApply(_, implRefTargs) = ann.args(0)
      var implRefTarg = implRefTargs(tparam.paramPos).tpe.typeSymbol
      val tpe = if (implRefTarg.isTypeParameterOrSkolem) {
        if (implRefTarg.owner == macroDef) {
          // [Eugene] doesn't work when macro def is compiled separately from its usages
          // then implRefTarg is not a skolem and isn't equal to any of macroDef.typeParams
//          val paramPos = implRefTarg.deSkolemize.paramPos
          val paramPos = macroDef.typeParams.indexWhere(_.name == implRefTarg.name)
          typeArgs(paramPos).tpe
        } else
          implRefTarg.tpe.asSeenFrom(
            if (prefixTree == EmptyTree) macroDef.owner.tpe else prefixTree.tpe,
            macroDef.owner)
      } else
        implRefTarg.tpe
      macroLogVerbose("resolved tparam %s as %s".format(tparam, tpe))
      resolved(tparam) = tpe
      param.tpe.typeSymbol match {
        case definitions.TypeTagClass =>
          // do nothing
        case definitions.ConcreteTypeTagClass =>
          if (!tpe.isConcrete) context.abort(context.enclosingPosition, "cannot create ConcreteTypeTag from a type %s having unresolved type parameters".format(tpe))
          // otherwise do nothing
        case _ =>
          throw new Error("unsupported tpe: " + tpe)
      }
      Some(tparam)
    })
    val tags = paramss.last takeWhile (_.isType) map (resolved(_)) map (tpe => {
      // generally speaking, it's impossible to calculate erasure from a tpe here
      // the tpe might be compiled by this run, so its jClass might not exist yet
      // hence I just pass `null` instead and leave this puzzle to macro programmers
      val ttag = TypeTag(tpe, null)
      if (ttag.isConcrete) ttag.toConcrete else ttag
    })
    if (paramss.lastOption map (params => !params.isEmpty && params.forall(_.isType)) getOrElse false) argss = argss :+ Nil
    argss = argss.dropRight(1) :+ (tags ++ argss.last) // todo. add support for context bounds in argss

    assert(argss.length == paramss.length, "argss: %s, paramss: %s".format(argss, paramss))
    val rawArgss = for ((as, ps) <- argss zip paramss) yield {
      if (isVarArgsList(ps)) as.take(ps.length - 1) :+ as.drop(ps.length - 1)
      else as
    }
    val rawArgs = rawArgss.flatten
    macroTraceVerbose("rawArgs: ")(rawArgs)
    Some(rawArgs)
  }

  /** Keeps track of macros in-flight.
   *  See more informations in comments to ``openMacros'' in ``scala.reflect.makro.Context''.
   */
  var openMacros = List[MacroContext]()
  def enclosingMacroPosition = openMacros map (_.macroApplication.pos) find (_ ne NoPosition) getOrElse NoPosition

  /** Performs macro expansion:
   *    1) Checks whether the expansion needs to be delayed (see ``mustDelayMacroExpansion'')
   *    2) Loads macro implementation using ``macroMirror''
   *    3) Synthesizes invocation arguments for the macro implementation
   *    4) Checks that the result is a tree bound to this universe
   *    5) Typechecks the result against the return type of the macro definition
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
   *    the fallback method invocation          if the expansion has been unsuccessful, but there is a fallback,
   *    the expandee unchanged                  if the expansion has been delayed,
   *    the expandee fully expanded             if the expansion has been delayed before and has been expanded now,
   *    the expandee with an error marker set   if the expansion has been cancelled due malformed arguments or implementation
   *    the expandee with an error marker set   if there has been an error
   */
  def macroExpand(typer: Typer, expandee: Tree, mode: Int = EXPRmode, pt: Type = WildcardType): Tree = {
    def fail(what: String, tree: Tree): Tree = {
      val err = typer.context.errBuffer.head
      this.fail(typer, tree, "failed to %s: %s at %s".format(what, err.errMsg, err.errPos))
      return expandee
    }
    val start = startTimer(macroExpandNanos)
    incCounter(macroExpandCount)
    try {
      macroExpand1(typer, expandee) match {
        case Success(expanded) =>
          try {
            var expectedTpe = expandee.tpe

            // [Eugene] weird situation. what's the conventional way to deal with it?
            val isNullaryInvocation = expandee match {
              case TypeApply(Select(_, _), _) => true
              case TypeApply(Ident(_), _) => true
              case Select(_, _) => true
              case Ident(_) => true
              case _ => false
            }
            if (isNullaryInvocation) expectedTpe match {
              case NullaryMethodType(restpe) =>
                macroTraceVerbose("nullary invocation of a nullary method. unwrapping expectedTpe from " + expectedTpe + " to: ")(restpe)
                expectedTpe = restpe
              case MethodType(Nil, restpe) =>
                macroTraceVerbose("nullary invocation of a method with an empty parameter list. unwrapping expectedTpe from " + expectedTpe + " to: ")(restpe)
                expectedTpe = restpe
              case _ => ;
            }

            macroLogVerbose("typechecking1 against %s: %s".format(expectedTpe, expanded))
            var typechecked = typer.context.withImplicitsEnabled(typer.typed(expanded, EXPRmode, expectedTpe))
            if (typer.context.hasErrors) fail("typecheck against macro def return type", expanded)
            macroLogVerbose("typechecked1:%n%s%n%s".format(typechecked, showRaw(typechecked)))

            macroLogVerbose("typechecking2 against %s: %s".format(pt, expanded))
            typechecked = typer.context.withImplicitsEnabled(typer.typed(typechecked, EXPRmode, pt))
            if (typer.context.hasErrors) fail("typecheck against expected type", expanded)
            macroLogVerbose("typechecked2:%n%s%n%s".format(typechecked, showRaw(typechecked)))

            typechecked
          } finally {
            openMacros = openMacros.tail
          }
        case Fallback(fallback) =>
          typer.context.withImplicitsEnabled(typer.typed(fallback, EXPRmode, pt))
        case Other(result) =>
          result
      }
    } finally {
      stopTimer(macroExpandNanos, start)
    }
  }

  private sealed abstract class MacroExpansionResult extends Product with Serializable
  private case class Success(expanded: Tree) extends MacroExpansionResult
  private case class Fallback(fallback: Tree) extends MacroExpansionResult
  private case class Other(result: Tree) extends MacroExpansionResult
  private def Delay(expanded: Tree) = Other(expanded)
  private def Skip(expanded: Tree) = Other(expanded)
  private def Cancel(expandee: Tree) = Other(expandee)
  private def Failure(expandee: Tree) = Other(expandee)
  private def fail(typer: Typer, expandee: Tree, msg: String = null) = {
    def msgForLog = if (msg != null && (msg contains "exception during macro expansion")) msg.split(EOL).drop(1).headOption.getOrElse("?") else msg
    macroLogLite("macro expansion has failed: %s".format(msgForLog))
    val pos = if (expandee.pos != NoPosition) expandee.pos else enclosingMacroPosition
    if (msg != null) typer.context.error(pos, msg)
    typer.infer.setError(expandee)
    Failure(expandee)
  }

  /** Does the same as ``macroExpand'', but without typechecking the expansion
   *  Meant for internal use within the macro infrastructure, don't use it elsewhere.
   */
  private def macroExpand1(typer: Typer, expandee: Tree): MacroExpansionResult =
    // InfoLevel.Verbose examines and prints out infos of symbols
    // by the means of this'es these symbols can climb up the lexical scope
    // when these symbols will be examined by a node printer
    // they will enumerate and analyze their children (ask for infos and tpes)
    // if one of those children involves macro expansion, things might get nasty
    // that's why I'm temporarily turning this behavior off
    withInfoLevel(nodePrinters.InfoLevel.Quiet) {
      // if a macro implementation is incompatible or any of the arguments are erroneous
      // there is no sense to expand the macro itself => it will only make matters worse
      if (expandee.symbol.isErroneous || (expandee exists (_.isErroneous))) {
        val reason = if (expandee.symbol.isErroneous) "incompatible macro implementation" else "erroneous arguments"
        macroTraceVerbose("cancelled macro expansion because of %s: ".format(reason))(expandee)
        return Cancel(typer.infer.setError(expandee))
      }

      macroRuntime(expandee.symbol) match {
        case Some(runtime) =>
          macroExpandWithRuntime(typer, expandee, runtime)
        case None =>
          macroExpandWithoutRuntime(typer, expandee)
      }
    }

  /** Expands a macro when a runtime (i.e. the macro implementation) can be successfully loaded
   *  Meant for internal use within the macro infrastructure, don't use it elsewhere.
   */
  private def macroExpandWithRuntime(typer: Typer, expandee: Tree, runtime: MacroRuntime): MacroExpansionResult = {
    def issueFreeError(sym: FreeSymbol) = {
      val template = (
          "Macro expansion contains free @kind@ variable %s. Have you forgotten to use %s? "
        + "If you have troubles tracking free @kind@ variables, consider using -Xlog-free-@kind@s"
      )
      val forgotten = (
        if (sym.isTerm) "eval when splicing this variable into a reifee"
        else "c.TypeTag annotation for this type parameter"
      )
      typer.context.error(expandee.pos,
        template.replaceAllLiterally("@kind@", sym.name.nameKind).format(
          sym.name + " " + sym.origin, forgotten)
      )
    }
    def macroExpandInternal = {
      val wasDelayed  = isDelayed(expandee)
      val undetparams = calculateUndetparams(expandee)
      val nowDelayed  = !typer.context.macrosEnabled || undetparams.nonEmpty

      def failExpansion(msg: String = null) = fail(typer, expandee, msg)
      def performExpansion(args: List[Any]): MacroExpansionResult = {
        val numErrors    = reporter.ERROR.count
        def hasNewErrors = reporter.ERROR.count > numErrors

        val expanded = runtime(args)

        if (hasNewErrors)
          failExpansion() // errors have been reported by the macro itself
        else expanded match {
          case expanded: Expr[_] =>
            macroLogVerbose("original:")
            macroLogLite("" + expanded.tree + "\n" + showRaw(expanded.tree))

            freeTerms(expanded.tree) foreach issueFreeError
            freeTypes(expanded.tree) foreach issueFreeError
            if (hasNewErrors) failExpansion()

            // inherit the position from the first position-ful expandee in macro callstack
            // this is essential for sane error messages
            // now macro expansion gets typechecked against the macro definition return type
            // however, this happens in macroExpand, not here in macroExpand1
            else Success(atPos(enclosingMacroPosition.focus)(expanded.tree))
          case _ =>
            failExpansion(
              "macro must return a compiler-specific expr; returned value is " + (
                if (expanded.isInstanceOf[Expr[_]]) " Expr, but it doesn't belong to this compiler's universe"
                else " of " + expanded.getClass
              )
            )
        }
      }

      if (wasDelayed) {
        if (nowDelayed) Delay(expandee)
        else Skip(macroExpandAll(typer, expandee))
      }
      else {
        macroLogLite("typechecking macro expansion %s at %s".format(expandee, expandee.pos))
        macroArgs(typer, expandee).fold(failExpansion(): MacroExpansionResult) {
          case args @ ((context: MacroContext) :: _) =>
            if (nowDelayed) {
              macroLogLite("macro expansion is delayed: %s".format(expandee))
              delayed += expandee -> undetparams
              // need to save typer context for `macroExpandAll`
              // need to save macro context to preserve enclosures
              expandee attach MacroAttachment(delayed = true, typerContext = typer.context, macroContext = Some(context))
              Delay(expandee)
            }
            else {
              // adding stuff to openMacros is easy, but removing it is a nightmare
              // it needs to be sprinkled over several different code locations
              // why? https://github.com/scala/scala/commit/bd3eacbae21f39b1ac7fe8ade4ed71fa98e1a28d#L2R1137
              // todo. will be improved
              openMacros ::= context
              var isSuccess = false
              try performExpansion(args) match {
                case x: Success => isSuccess = true ; x
                case x          => x
              }
              finally {
                expandee.detach(classOf[MacroAttachment])
                if (!isSuccess) openMacros = openMacros.tail
              }
            }
        }
      }
    }

    try macroExpandInternal
    catch { case ex => handleMacroExpansionException(typer, expandee, ex) }
  }

  private def macroExpandWithoutRuntime(typer: Typer, expandee: Tree): MacroExpansionResult = {
    val macroDef = expandee.symbol
    def notFound() = {
      typer.context.error(expandee.pos, "macro implementation not found: " + macroDef.name + " " +
        "(the most common reason for that is that you cannot use macro implementations in the same compilation run that defines them)\n" +
        "if you do need to define macro implementations along with the rest of your program, consider two-phase compilation with -Xmacro-fallback-classpath " +
        "in the second phase pointing to the output of the first phase")
      None
    }
    def fallBackToOverridden(tree: Tree): Option[Tree] = {
      tree match {
        case Select(qual, name) if (macroDef.isTermMacro) =>
          macroDef.allOverriddenSymbols match {
            case first :: _ =>
              Some(Select(qual, name) setPos tree.pos setSymbol first)
            case _ =>
              macroTraceVerbose("macro is not overridden: ")(tree)
              notFound()
          }
        case Apply(fn, args) =>
          fallBackToOverridden(fn) match {
            case Some(fn1) => Some(Apply(fn1, args) setPos tree.pos)
            case _         => None
          }
        case TypeApply(fn, args) =>
          fallBackToOverridden(fn) match {
            case Some(fn1) => Some(TypeApply(fn1, args) setPos tree.pos)
            case _         => None
          }
        case _ =>
          macroTraceVerbose("unexpected tree in fallback: ")(tree)
          notFound()
      }
    }
    fallBackToOverridden(expandee) match {
      case Some(tree1) =>
        macroTraceLite("falling back to: ")(tree1)
        currentRun.macroExpansionFailed = true
        Fallback(tree1)
      case None =>
        fail(typer, expandee)
    }
  }

  private def handleMacroExpansionException(typer: Typer, expandee: Tree, ex: Throwable): MacroExpansionResult = {
    // [Eugene] any ideas about how to improve this one?
    val realex = ReflectionUtils.unwrapThrowable(ex)
    realex match {
      case realex: reflect.makro.runtime.AbortMacroException =>
        macroLogVerbose("macro expansion has failed: %s".format(realex.msg))
        fail(typer, expandee) // error has been reported by abort
      case err: TypeError =>
        macroLogLite("macro expansion has failed: %s at %s".format(err.msg, err.pos))
        throw err // error should be propagated, don't report
      case _ =>
        val message = {
          try {
            // [Eugene] is there a better way?
            // [Paul] See Exceptional.scala and Origins.scala.
            val relevancyThreshold = realex.getStackTrace().indexWhere(este => este.getMethodName == "macroExpand1")
            if (relevancyThreshold == -1) None
            else {
              var relevantElements = realex.getStackTrace().take(relevancyThreshold + 1)
              def isMacroInvoker(este: StackTraceElement) = este.isNativeMethod || (este.getClassName != null && (este.getClassName contains "fastTrack"))
              var threshold = relevantElements.reverse.indexWhere(isMacroInvoker) + 1
              while (threshold != relevantElements.length && isMacroInvoker(relevantElements(relevantElements.length - threshold - 1))) threshold += 1
              relevantElements = relevantElements dropRight threshold

              realex.setStackTrace(relevantElements)
              val message = new java.io.StringWriter()
              realex.printStackTrace(new java.io.PrintWriter(message))
              Some(EOL + message)
            }
          } catch {
            // if the magic above goes boom, just fall back to uninformative, but better than nothing, getMessage
            case ex: Throwable =>
              None
          }
        } getOrElse realex.getMessage
        fail(typer, expandee, "exception during macro expansion: " + message)
    }
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
  private val delayed = perRunCaches.newWeakMap[Tree, collection.mutable.Set[Int]]
  private def isDelayed(expandee: Tree) = delayed contains expandee
  private def calculateUndetparams(expandee: Tree): collection.mutable.Set[Int] =
    delayed.get(expandee).getOrElse {
      val calculated = collection.mutable.Set[Symbol]()
      expandee foreach (sub => {
        def traverse(sym: Symbol) = if (sym != null && (undetparams contains sym.id)) calculated += sym
        if (sub.symbol != null) traverse(sub.symbol)
        if (sub.tpe != null) sub.tpe foreach (sub => traverse(sub.typeSymbol))
      })
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
   *  See the documentation for ``macroExpand'' for more information.
   */
  def macroExpandAll(typer: Typer, expandee: Tree): Tree =
    new Transformer {
      override def transform(tree: Tree) = super.transform(tree match {
        // todo. expansion should work from the inside out
        case wannabe if (delayed contains wannabe) && calculateUndetparams(wannabe).isEmpty =>
          val context = wannabe.attachment[MacroAttachment].typerContext
          delayed -= wannabe
          context.implicitsEnabled = typer.context.implicitsEnabled
          context.enrichmentEnabled = typer.context.enrichmentEnabled
          context.macrosEnabled = typer.context.macrosEnabled
          macroExpand(newTyper(context), wannabe, EXPRmode, WildcardType)
        case _ =>
          tree
      })
    }.transform(expandee)
}
