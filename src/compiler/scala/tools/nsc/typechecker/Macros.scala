/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package typechecker

import java.lang.Math.min

import symtab.Flags._
import scala.annotation._
import scala.reflect.internal.TypesStats
import scala.reflect.internal.util.ListOfNil
import scala.reflect.internal.util.ScalaClassLoader
import scala.reflect.internal.util.Statistics
import scala.reflect.macros.compiler.DefaultMacroCompiler
import scala.reflect.macros.runtime.{AbortMacroException, MacroRuntimes}
import scala.reflect.macros.util._
import scala.reflect.runtime.ReflectionUtils
import scala.tools.reflect.FastTrack
import scala.util.control.ControlThrowable
import scala.util.control.NonFatal
import Fingerprint._

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
 *           (c: scala.reflect.macros.blackbox.Context)
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
trait Macros extends MacroRuntimes with Traces with Helpers {
  self: Analyzer =>

  import global._
  import definitions._
  import treeInfo.{isRepeatedParamType => _, _}

  lazy val fastTrack = new FastTrack[self.type](self)

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
   *  (because they refer to macro impls, and macro impls refer to *box.Context defined in scala-reflect.jar).
   *  More details can be found in comments to https://github.com/scala/bug/issues/5940.
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
  case class MacroImplBinding(
      // Is this macro impl a bundle (a trait extending *box.Macro) or a vanilla def?
      isBundle: Boolean,
      // Is this macro impl blackbox (i.e. having blackbox.Context in its signature)?
      isBlackbox: Boolean,
      // Java class name of the class that contains the macro implementation
      // is used to load the corresponding object with Java reflection
      className: String,
      // method name of the macro implementation
      // `className` and `methName` are all we need to reflectively invoke a macro implementation
      // because macro implementations cannot be overloaded
      methName: String,
      // flattens the macro impl's parameter lists having symbols replaced with their fingerprints
      // currently fingerprints are calculated solely from types of the symbols:
      //   * c.Expr[T] => LiftedTyped
      //   * c.Tree => LiftedUntyped
      //   * c.WeakTypeTag[T] => Tagged(index of the type parameter corresponding to that type tag)
      //   * everything else (e.g. *box.Context) => Other
      // f.ex. for: def impl[T: WeakTypeTag, U, V: WeakTypeTag](c: blackbox.Context)(x: c.Expr[T], y: c.Tree): (U, V) = ???
      // `signature` will be equal to List(List(Other), List(LiftedTyped, LiftedUntyped), List(Tagged(0), Tagged(2)))
      signature: List[List[Fingerprint]],
      // type arguments part of a macro impl ref (the right-hand side of a macro definition)
      // these trees don't refer to a macro impl, so we can pickle them as is
      targs: List[Tree]) {
    // Was this binding derived from a `def ... = macro ???` definition?
    def is_??? = {
      val Predef_??? = currentRun.runDefinitions.Predef_???
      className == Predef_???.owner.javaClassName && methName == Predef_???.name.encoded
    }
    def isWhitebox = !isBlackbox
  }

  /** Macro def -> macro impl bindings are serialized into a `macroImpl` annotation
   *  with synthetic content that carries the payload described in `MacroImplBinding`.
   *
   *  For example, for a pair of macro definition and macro implementation:
   *    def impl(c: scala.reflect.macros.blackbox.Context): c.Expr[Unit] = ???
   *    def foo: Unit = macro impl
   *
   *  We will have the following annotation added on the macro definition `foo`:
   *
   *  {{{
   *    @scala.reflect.macros.internal.macroImpl(
   *      `macro`(
   *        "macroEngine" = <current macro engine>,
   *        "isBundle" = false,
   *        "isBlackbox" = true,
   *        "signature" = List(Other),
   *        "methodName" = "impl",
   *        "className" = "Macros$"))
   *  }}}
   */
  def macroEngine = "v7.0 (implemented in Scala 2.11.0-M8)"
  object MacroImplBinding {
    def pickleAtom(obj: Any): Tree = obj match {
      case list: List[_]  => Apply(Ident(ListModule), list map pickleAtom)
      case s: String      => Literal(Constant(s))
      case d: Double      => Literal(Constant(d))
      case b: Boolean     => Literal(Constant(b))
      case f: Fingerprint => Literal(Constant(f.value))
      case x              => throw new MatchError(x)
    }

    def unpickleAtom(tree: Tree): Any = tree match {
      case Apply(list @ Ident(_), args) if list.symbol == ListModule => args map unpickleAtom
      case Literal(Constant(s: String))                              => s
      case Literal(Constant(d: Double))                              => d
      case Literal(Constant(b: Boolean))                             => b
      case Literal(Constant(i: Int))                                 => Fingerprint(i)
      case x                                                         => throw new MatchError(x)
    }

    def extractMacroBindingImpl(macroImplRef: Tree): MacroImplBinding = {
      val runDefinitions = currentRun.runDefinitions
      import runDefinitions._
      val MacroImplReference(isBundle, isBlackbox, owner, macroImpl, targs) = (macroImplRef: @unchecked)

      // todo. refactor when fixing scala/bug#5498
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

      def signature: List[List[Fingerprint]] = {
        def fingerprint(tpe: Type): Fingerprint = tpe.dealiasWiden match {
          case TypeRef(_, RepeatedParamClass, underlying :: Nil) => fingerprint(underlying)
          case ExprClassOf(_) => LiftedTyped
          case TreeType() => LiftedUntyped
          case _ => Other
        }

        val transformed = transformTypeTagEvidenceParams(macroImplRef, (_, tparam) => tparam)
        mmap(transformed)(p => if (p.isTerm) fingerprint(p.info) else Tagged(p.paramPos))
      }

      MacroImplBinding(isBundle, isBlackbox, className, macroImpl.name.toString, signature, targs map (_.duplicate))
    }

    def pickle(macroImplRef: Tree): Tree = {

      val MacroImplBinding(isBundle, isBlackbox, className, methodName, signature, targs) =
        extractMacroBindingImpl(macroImplRef)

      val payload = List[(String, Any)](
        "macroEngine" -> macroEngine,
        "isBundle"    -> isBundle,
        "isBlackbox"  -> isBlackbox,
        "className"   -> className,
        "methodName"  -> methodName,
        "signature"   -> signature
      )

      // the shape of the nucleus is chosen arbitrarily. it doesn't carry any payload.
      // it's only necessary as a stub `fun` for an Apply node that carries metadata in its `args`
      // so don't try to find a program element named "macro" that corresponds to the nucleus
      // I just named it "macro", because it's macro-related, but I could as well name it "foobar"
      val nucleus = Ident(newTermName("macro"))
      val wrapped = Apply(nucleus, payload map { case (k, v) => Assign(pickleAtom(k), pickleAtom(v)) })
      val pickle = gen.mkTypeApply(wrapped, targs)

      // assign NoType to all freshly created AST nodes
      // otherwise pickler will choke on tree.tpe being null
      // there's another gotcha
      // if you don't assign a ConstantType to a constant
      // then pickling will crash
      new AstTransformer {
        override def transform(tree: Tree) = {
          tree match {
            case Literal(const @ Constant(_)) if tree.tpe == null => tree.setType(ConstantType(const))
            case _ if tree.tpe == null => tree.setType(NoType)
            case _ =>
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
      val Apply(_, pickledPayload) = wrapped: @unchecked
      val payload = pickledPayload.map { case Assign(k, v) => (unpickleAtom(k), unpickleAtom(v)) case x => throw new MatchError(x) }.toMap

      // TODO: refactor error handling: fail always throws a TypeError,
      // and uses global state (analyzer.lastTreeToTyper) to determine the position for the error
      def fail(msg: String) = MacroCantExpandIncompatibleMacrosError(msg)
      def unpickle[T](field: String, clazz: Class[T]): T = {
        def failField(msg: String) = fail(s"$field $msg")
        if (!payload.contains(field)) failField("is supposed to be there")
        val raw: Any = payload(field)
        if (raw == null) failField(s"is not supposed to be null")
        val expected = box(clazz)
        val actual = raw.getClass
        if (!expected.isAssignableFrom(actual)) failField(s"has wrong type: expected $expected, actual $actual")
        raw.asInstanceOf[T]
      }

      if (!payload.contains("macroEngine")) MacroCantExpand210xMacrosError("macroEngine field not found")
      val macroEngine = unpickle("macroEngine", classOf[String])
      if (self.macroEngine != macroEngine) MacroCantExpandIncompatibleMacrosError(s"expected = ${self.macroEngine}, actual = $macroEngine")

      val isBundle = unpickle("isBundle", classOf[Boolean])
      val isBlackbox = unpickle("isBlackbox", classOf[Boolean])
      val className = unpickle("className", classOf[String])
      val methodName = unpickle("methodName", classOf[String])
      val signature = unpickle("signature", classOf[List[List[Fingerprint]]])
      MacroImplBinding(isBundle, isBlackbox, className, methodName, signature, targs)
    }

    private def box[T](clazz: Class[T]): Class[_] = clazz match {
      case java.lang.Byte.TYPE => classOf[java.lang.Byte]
      case java.lang.Short.TYPE => classOf[java.lang.Short]
      case java.lang.Character.TYPE => classOf[java.lang.Character]
      case java.lang.Integer.TYPE => classOf[java.lang.Integer]
      case java.lang.Long.TYPE => classOf[java.lang.Long]
      case java.lang.Float.TYPE => classOf[java.lang.Float]
      case java.lang.Double.TYPE => classOf[java.lang.Double]
      case java.lang.Void.TYPE => classOf[scala.runtime.BoxedUnit]
      case java.lang.Boolean.TYPE => classOf[java.lang.Boolean]
      case _ => clazz
    }
  }

  def bindMacroImpl(macroDef: Symbol, macroImplRef: Tree): Unit = {
    val pickle = MacroImplBinding.pickle(macroImplRef)
    macroDef withAnnotation AnnotationInfo(MacroImplAnnotation.tpe, List(pickle), Nil)
  }

  def loadMacroImplBinding(macroDef: Symbol): Option[MacroImplBinding] = {
    macroImplBindingCache.getOrElseUpdate(macroDef,
      macroDef.getAnnotation(MacroImplAnnotation) collect {
        case AnnotationInfo(_, List(pickle), _) => MacroImplBinding.unpickle(pickle)
      } orElse {
        macroDef.getAnnotation(MacroImplLocationAnnotation) collect {
          case AnnotationInfo(_, List(macroImplRef), _) => MacroImplBinding.extractMacroBindingImpl(macroImplRef)
        }
      }
    )
  }
  private val macroImplBindingCache = perRunCaches.newMap[Symbol, Option[MacroImplBinding]]()

  def isBlackbox(expandee: Tree): Boolean = isBlackbox(dissectCore(expandee).symbol)
  def isBlackbox(macroDef: Symbol): Boolean = pluginsIsBlackbox(macroDef)

  /** Default implementation of `isBlackbox`.
   *  Can be overridden by analyzer plugins (see AnalyzerPlugins.pluginsIsBlackbox for more details)
   */
  def standardIsBlackbox(macroDef: Symbol): Boolean = {
    val fastTrackBoxity = fastTrack.get(macroDef).map(_.isBlackbox)
    val bindingBoxity = loadMacroImplBinding(macroDef).map(_.isBlackbox)
    fastTrackBoxity orElse bindingBoxity getOrElse false
  }

  /** Verifies that the body of a macro def typechecks to a reference to a static public non-overloaded method or a top-level macro bundle,
   *  and that that method is signature-wise compatible with the given macro definition.
   *
   *  @return Macro impl reference for the given macro definition if everything is okay.
   *          EmptyTree if an error occurs.
   */
  def typedMacroBody(typer: Typer, macroDdef: DefDef): Tree = pluginsTypedMacroBody(typer, macroDdef)

  /** Default implementation of `typedMacroBody`.
   *  Can be overridden by analyzer plugins (see AnalyzerPlugins.pluginsTypedMacroBody for more details)
   */
  def standardTypedMacroBody(typer: Typer, macroDdef: DefDef): Tree = {
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

      if (!typer.checkFeature(macroDdef.pos, currentRun.runDefinitions.MacrosFeature, immediate = true)) {
        macroLogVerbose("typecheck terminated unexpectedly: language.experimental.macros feature is not enabled")
        fail()
      } else {
        val macroDdef1: macroDdef.type = macroDdef
        val typer1: typer.type = typer
        val macroCompiler = new {
          val global: self.global.type = self.global
          val typer: self.global.analyzer.Typer = typer1.asInstanceOf[self.global.analyzer.Typer]
          val macroDdef: self.global.DefDef = macroDdef1
        } with DefaultMacroCompiler
        val macroImplRef = macroCompiler.resolveMacroImpl
        if (macroImplRef.isEmpty) fail() else {
          def hasTypeTag = {
            val marker = NoSymbol.newErrorValue(TermName("restricted"))
            val xformed = transformTypeTagEvidenceParams(macroImplRef, (_, _) => marker)
            xformed.nonEmpty && xformed.last.contains(marker)
          }
          if (macroDdef.name == nme.macroTransform && hasTypeTag) {
            typer.context.error(macroDdef.pos, "implementation restriction: macro annotation impls cannot have typetag context bounds " +
                                               "(consider taking apart c.macroApplication and manually calling c.typecheck on the type arguments)")
            fail()
          } else success(macroImplRef)
        }
      }
    }
  }

  def macroContext(typer: Typer, prefixTree: Tree, expandeeTree: Tree): MacroContext = {
    new {
      val universe: self.global.type = self.global
      val callsiteTyper: universe.analyzer.Typer = typer.asInstanceOf[global.analyzer.Typer]
      val expandee = universe.analyzer.macroExpanderAttachment(expandeeTree).original orElse duplicateAndKeepPositions(expandeeTree)
    } with UnaffiliatedMacroContext {
      val prefix = Expr[Nothing](prefixTree)(TypeTag.Nothing)
      override def toString = "MacroContext(%s@%s +%d)".format(expandee.symbol.name, expandee.pos, enclosingMacros.length - 1 /* exclude myself */)
    }
  }

  /** Calculate the arguments to pass to a macro implementation when expanding the provided tree.
   */
  case class MacroArgs(c: MacroContext, others: List[Any])
  def macroArgs(typer: Typer, expandee: Tree): MacroArgs = pluginsMacroArgs(typer, expandee)

  /** Default implementation of `macroArgs`.
   *  Can be overridden by analyzer plugins (see AnalyzerPlugins.pluginsMacroArgs for more details)
   */
  def standardMacroArgs(typer: Typer, expandee: Tree): MacroArgs = {
    val macroDef = expandee.symbol
    val paramss = macroDef.paramss
    val treeInfo.Applied(core, targs, argss) = expandee
    val prefix = core match { case Select(qual, _) => qual; case _ => EmptyTree }
    val context = expandee.attachments.get[MacroRuntimeAttachment].flatMap(_.macroContext).getOrElse(macroContext(typer, prefix, expandee))

    macroLogVerbose(sm"""
      |context: $context
      |prefix: $prefix
      |targs: $targs
      |argss: $argss
      |paramss: $paramss
    """.trim)

    import typer.TyperErrorGen._
    val isNullaryArgsEmptyParams = argss.isEmpty && paramss == ListOfNil
    if (paramss.sizeCompare(argss) < 0) MacroTooManyArgumentListsError(expandee)
    if (paramss.sizeCompare(argss) > 0 && !isNullaryArgsEmptyParams) MacroTooFewArgumentListsError(expandee)

    val macroImplArgs: List[Any] =
      if (fastTrack contains macroDef) {
        // Take a dry run of the fast track implementation
        if (fastTrack(macroDef) validate expandee) argss.flatten
        else MacroTooFewArgumentListsError(expandee)
      }
      else {
        def calculateMacroArgs(binding: MacroImplBinding) = {
          val signature = if (binding.isBundle) binding.signature else binding.signature.tail
          macroLogVerbose(s"binding: $binding")

          // STEP I: prepare value arguments of the macro expansion
          // wrap argss in c.Expr if necessary (i.e. if corresponding macro impl param is of type c.Expr[T])
          // expand varargs (nb! varargs can apply to any parameter section, not necessarily to the last one)
          val trees = map3(argss, paramss, signature)((args, defParams, implParams) => {
            val isVarargs = isVarArgsList(defParams)
            if (isVarargs) {
              if (defParams.lengthIs > (args.length + 1)) MacroTooFewArgumentsError(expandee)
            } else {
              if (defParams.sizeCompare(args) < 0) MacroTooManyArgumentsError(expandee)
              if (defParams.sizeCompare(args) > 0) MacroTooFewArgumentsError(expandee)
            }

            val wrappedArgs = mapWithIndex(args)((arg, j) => {
              val fingerprint = implParams(min(j, implParams.length - 1))
              val duplicatedArg = duplicateAndKeepPositions(arg)
              fingerprint match {
                case LiftedTyped => context.Expr[Nothing](duplicatedArg)(TypeTag.Nothing) // TODO: scala/bug#5752
                case LiftedUntyped => duplicatedArg
                case _ => abort(s"unexpected fingerprint $fingerprint in $binding with paramss being $paramss " +
                                s"corresponding to arg $arg in $argss")
              }
            })

            if (isVarargs) {
              val (normal, varargs) = wrappedArgs splitAt (defParams.length - 1)
              normal :+ varargs // pack all varargs into a single Seq argument (varargs Scala style)
            } else wrappedArgs
          })
          macroLogVerbose(s"trees: $trees")

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
          val tags = signature.flatten collect { case f if f.isTag => f.paramPos } map (paramPos => {
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
          macroLogVerbose(s"tags: $tags")

          // if present, tags always come in a separate parameter/argument list
          // that's because macro impls can't have implicit parameters other than c.WeakTypeTag[T]
          (trees :+ tags).flatten
        }

        val binding = loadMacroImplBinding(macroDef).get
        if (binding.is_???) Nil
        else calculateMacroArgs(binding)
      }
    macroLogVerbose(s"macroImplArgs: $macroImplArgs")
    MacroArgs(context, macroImplArgs)
  }

  /** Keeps track of macros in-flight.
   *  See more informations in comments to `openMacros` in `scala.reflect.macros.whitebox.Context`.
   */
  var _openMacros = List[MacroContext]()
  def openMacros = _openMacros
  def pushMacroContext(c: MacroContext) = _openMacros ::= c
  def popMacroContext() = _openMacros = _openMacros.tail
  def enclosingMacroPosition = openMacros map (_.macroApplication.pos) find (_ ne NoPosition) getOrElse NoPosition

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
   *  ========= Macro expansion =========
   *
   *  First of all `macroExpandXXX`:
   *    1) If necessary desugars the `expandee` to fit into the default expansion scheme
   *       that is understood by `macroExpandWithRuntime` / `macroExpandWithoutRuntime`
   *
   *  Then `macroExpandWithRuntime`:
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
  abstract class MacroExpander(val typer: Typer, val expandee: Tree) {
    def onSuccess(expanded: Tree): Tree
    def onFallback(expanded: Tree): Tree
    def onSuppressed(expandee: Tree): Tree = expandee
    def onDelayed(expanded: Tree): Tree = expanded
    def onSkipped(expanded: Tree): Tree = expanded
    def onFailure(@unused expanded: Tree): Tree = { typer.infer.setError(expandee); expandee }

    def apply(desugared: Tree): Tree = {
      if (isMacroExpansionSuppressed(desugared)) onSuppressed(expandee)
      else expand(desugared)
    }

    protected def expand(desugared: Tree): Tree = {
      def positionsToOffset(expanded: Tree) = {
        // Macros might have spliced arguments with range positions into non-compliant
        // locations, notably, under a tree without a range position. Or, they might
        // splice a tree that `resetAttrs` has assigned NoPosition.
        //
        // Here, we just convert all positions in the tree to offset positions, and
        // convert NoPositions to something sensible.
        //
        // Given that the IDE now sees the expandee (by using -Ymacro-expand:discard),
        // this loss of position fidelity shouldn't cause any real problems.
        //
        // Alternatively, we could pursue a way to exclude macro expansions from position
        // invariant checking, or find a way not to touch expansions that happen to validate.
        //
        // This would be useful for cases like:
        //
        //    macro1 { macro2 { "foo" } }
        //
        // to allow `macro1` to see the range position of the "foo".
        val expandedPos = enclosingMacroPosition.focus
        def fixPosition(pos: Position) =
          if (pos == NoPosition) expandedPos else pos.focus
        expanded.foreach(t => t.pos = fixPosition(t.pos))
        expanded
      }
      def showDetailed(tree: Tree) = showRaw(tree, printIds = true, printTypes = true)
      def summary() = s"expander = $this, expandee = ${showDetailed(expandee)}, desugared = ${if (expandee == desugared) () else showDetailed(desugared)}"
      if (macroDebugVerbose) println(s"macroExpand: ${summary()}")
      linkExpandeeAndDesugared(expandee, desugared)

      val start = if (settings.areStatisticsEnabled) statistics.startTimer(statistics.macroExpandNanos) else null
      if (settings.areStatisticsEnabled) statistics.incCounter(statistics.macroExpandCount)
      try {
        withInfoLevel(nodePrinters.InfoLevel.Quiet) { // verbose printing might cause recursive macro expansions
          if (expandee.symbol.isErroneous || (expandee exists (_.isErroneous))) {
            val reason = if (expandee.symbol.isErroneous) "not found or incompatible macro implementation" else "erroneous arguments"
            macroLogVerbose(s"cancelled macro expansion because of $reason: $expandee")
            onFailure(typer.infer.setError(expandee))
          } else try {
            val expanded = {
              val runtime = macroRuntime(expandee)
              if (runtime != null) macroExpandWithRuntime(typer, expandee, runtime)
              else macroExpandWithoutRuntime(typer, expandee)
            }
            expanded match {
              case Success(expanded) =>
                // duplicate expanded tree to avoid structural sharing in macro-generated trees
                // see https://groups.google.com/group/scala-internals/browse_thread/thread/492560d941b315cc
                val expanded1 = try onSuccess(positionsToOffset(duplicateAndKeepPositions(expanded))) finally popMacroContext()
                if (!hasMacroExpansionAttachment(expanded1)) linkExpandeeAndExpanded(expandee, expanded1)
                if (settings.Ymacroexpand.value == settings.MacroExpand.Discard && !typer.context.isSearchingForImplicitParam) {
                  suppressMacroExpansion(expandee)
                  expandee.setType(expanded1.tpe)
                }
                else expanded1
              case Fallback(fallback) => onFallback(fallback)
              case Delayed(delayed) => onDelayed(delayed)
              case Skipped(skipped) => onSkipped(skipped)
              case Failure(failure) => onFailure(failure)
            }
          } catch {
            case typer.TyperErrorGen.MacroExpansionException => onFailure(expandee)
          }
        }
      } finally {
        if (settings.areStatisticsEnabled) statistics.stopTimer(statistics.macroExpandNanos, start)
      }
    }
  }

  /** Expands a term macro used in apply role as `M(2)(3)` in `val x = M(2)(3)`.
   *  @param outerPt Expected type that comes from enclosing context (something that's traditionally called `pt`).
   *  @param innerPt Expected type that comes from the signature of a macro def, possibly wildcarded to help type inference.
   */
  class DefMacroExpander(typer: Typer, expandee: Tree, mode: Mode, outerPt: Type)
  extends MacroExpander(typer, expandee) {
    lazy val innerPt = {
      val tp = if (isNullaryInvocation(expandee)) expandee.tpe.finalResultType else expandee.tpe
      if (isBlackbox(expandee)) tp
      else {
        // approximation is necessary for whitebox macros to guide type inference
        // read more in the comments for onDelayed below
        val undetparams = tp collect { case tp1 if tp1.typeSymbol.isTypeParameter => tp1.typeSymbol }
        deriveTypeWithWildcards(undetparams)(tp)
      }
    }
    override def onSuccess(expanded0: Tree) = {
      // prematurely annotate the tree with a macro expansion attachment
      // so that adapt called indirectly by typer.typed knows that it needs to apply the existential fixup
      linkExpandeeAndExpanded(expandee, expanded0)

      def typecheck(label: String, tree: Tree, pt: Type): Tree = {
        if (tree.isErrorTyped) tree
        else {
          if (macroDebugVerbose) println(s"$label (against pt = $pt): $tree")
          // `macroExpandApply` is called from `adapt`, where implicit conversions are disabled
          // therefore we need to re-enable the conversions back temporarily
          val result = typer.context.withImplicitsEnabled(typer.typed(tree, mode, pt))
          if (result.isErrorTyped && macroDebugVerbose) println(s"$label has failed: ${typer.context.reporter.errors}")
          result
        }
      }

      if (isBlackbox(expandee)) {
        val expanded1 = atPos(enclosingMacroPosition.makeTransparent)(Typed(expanded0, TypeTree(innerPt)))
        typecheck("blackbox typecheck", expanded1, outerPt)
      } else {
        // whitebox expansions need to be typechecked against WildcardType first in order to avoid scala/bug#6992 and scala/bug#8048
        // then we typecheck against innerPt, not against outerPt in order to prevent scala/bug#8209
        val expanded1 = typecheck("whitebox typecheck #0", expanded0, WildcardType)
        val expanded2 = typecheck("whitebox typecheck #1", expanded1, innerPt)
        typecheck("whitebox typecheck #2", expanded2, outerPt)
      }
    }
    override def onDelayed(delayed: Tree) = {
      // =========== THE SITUATION ===========
      //
      // If we've been delayed (i.e. bailed out of the expansion because of undetermined type params present in the expandee),
      // then there are two possible situations we're in:
      // 1) We're in POLYmode, when the typer tests the waters wrt type inference
      // (e.g. as in typedArgToPoly in doTypedApply).
      // 2) We're out of POLYmode, which means that the typer is out of tricks to infer our type
      // (e.g. if we're an argument to a function call, then this means that no previous argument lists
      // can determine our type variables for us).
      //
      // Situation #1 is okay for us, since there's no pressure. In POLYmode we're just verifying that
      // there's nothing outrageously wrong with our undetermined type params (from what I understand!).
      //
      // Situation #2 requires measures to be taken. If we're in it, then noone's going to help us infer
      // the undetermined type params. Therefore we need to do something ourselves or otherwise this
      // expandee will forever remain not expanded (see scala/bug#5692). A traditional way out of this conundrum
      // is to call `instantiate` and let the inferencer try to find the way out. It works for simple cases,
      // but sometimes, if the inferencer lacks information, it will be forced to approximate.
      //
      // =========== THE PROBLEM ===========
      //
      // Consider the following example (thanks, Miles!):
      //
      // Iso represents an isomorphism between two datatypes:
      // 1) An arbitrary one (e.g. a random case class)
      // 2) A uniform representation for all datatypes (e.g. an HList)
      //
      //   trait Iso[T, U] {
      //   def to(t : T) : U
      //   def from(u : U) : T
      //   }
      //   implicit def materializeIso[T, U]: Iso[T, U] = macro ???
      //
      //   case class Foo(i: Int, s: String, b: Boolean)
      //   def foo[C, L](c: C)(implicit iso: Iso[C, L]): L = iso.to(c)
      //   foo(Foo(23, "foo", true))
      //
      // In the snippet above, even though we know that there's a fundep going from T to U
      // (in a sense that a datatype's uniform representation is unambiguously determined by the data type,
      // e.g. for Foo it will be Int :: String :: Boolean :: HNil), there's no way to convey this information
      // to the typechecker. Therefore the typechecker will infer Nothing for L, which is hardly what we want.
      //
      // =========== THE SOLUTION (ENABLED ONLY FOR WHITEBOX MACROS) ===========
      //
      // To give materializers a chance to say their word before vanilla inference kicks in,
      // we infer as much as possible (e.g. in the example above even though L is hopeless, C still can be inferred to Foo)
      // and then trigger macro expansion with the undetermined type parameters still there.
      // Thanks to that the materializer can take a look at what's going on and react accordingly.
      val shouldInstantiate = typer.context.undetparams.nonEmpty && !mode.inPolyMode
      if (shouldInstantiate) {
        if (isBlackbox(expandee)) typer.instantiatePossiblyExpectingUnit(delayed, mode, outerPt)
        else {
          forced += delayed
          typer.infer.inferExprInstance(delayed, typer.context.extractUndetparams(), outerPt, keepNothings = false)
          macroExpand(typer, delayed, mode, outerPt)
        }
      } else delayed
    }
    override def onFallback(fallback: Tree) = typer.typed(fallback, mode, outerPt)
  }

  /** Expands a term macro used in apply role as `M(2)(3)` in `val x = M(2)(3)`.
   *  @see DefMacroExpander
   */
  def macroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Tree = {
    // By default, use the current typer's fresh name creator in macros. The compiler option
    // allows people to opt in to the old behaviour of Scala 2.12, which used a global fresh creator.
    if (!settings.YmacroFresh.value) currentFreshNameCreator = typer.fresh
    val macroSym = expandee.symbol
    currentRun.profiler.beforeMacroExpansion(macroSym)
    try {
      pluginsMacroExpand(typer, expandee, mode, pt)
    } finally {
      currentRun.profiler.afterMacroExpansion(macroSym)
    }
  }

  /** Default implementation of `macroExpand`.
   *  Can be overridden by analyzer plugins (see AnalyzerPlugins.pluginsMacroExpand for more details)
   */
  def standardMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Tree = {
    val expander = new DefMacroExpander(typer, expandee, mode, pt)
    expander(expandee)
  }

  sealed abstract class MacroStatus(val result: Tree)
  case class Success(expanded: Tree) extends MacroStatus(expanded)
  case class Fallback(fallback: Tree) extends MacroStatus(fallback) { runReporting.seenMacroExpansionsFallingBack = true }
  case class Delayed(delayed: Tree) extends MacroStatus(delayed)
  case class Skipped(skipped: Tree) extends MacroStatus(skipped)
  case class Failure(failure: Tree) extends MacroStatus(failure)
  def Delay(expanded: Tree) = Delayed(expanded)
  def Skip(expanded: Tree) = Skipped(expanded)

  /** Expands a macro when a runtime (i.e. the macro implementation) can be successfully loaded
   *  Meant for internal use within the macro infrastructure, don't use it elsewhere.
   */
  def macroExpandWithRuntime(typer: Typer, expandee: Tree, runtime: MacroRuntime): MacroStatus = {
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
          val numErrors    = reporter.errorCount
          def hasNewErrors = reporter.errorCount > numErrors
          val expanded = { pushMacroContext(args.c); runtime(args) }
          if (hasNewErrors) MacroGeneratedTypeError(expandee)
          def validateResultingTree(expanded: Tree) = {
            macroLogVerbose("original:")
            macroLogLite("" + expanded + "\n" + showRaw(expanded))
            val freeSyms = expanded.freeSyms
            freeSyms foreach (sym => MacroFreeSymbolError(expandee, sym))
            val result = atPos(enclosingMacroPosition.focus)(expanded)
            Success(result)
          }
          expanded match {
            case expanded: Expr[_] if expandee.symbol.isTermMacro => validateResultingTree(expanded.tree)
            case expanded: Tree if expandee.symbol.isTermMacro    => validateResultingTree(expanded)
            case _ => MacroExpansionHasInvalidTypeError(expandee, expanded)
          }
        } catch {
          case t: Throwable =>
            if (openMacros.nonEmpty) popMacroContext() // weirdly we started popping on an empty stack when refactoring fatalWarnings logic
            val realex = ReflectionUtils.unwrapThrowable(t)
            realex match {
              case ex: InterruptedException => throw ex
              case ex: AbortMacroException => MacroGeneratedAbort(expandee, ex)
              case ex: ControlThrowable => throw ex
              case ex: TypeError => MacroGeneratedTypeError(expandee, ex)
              case NonFatal(_) => MacroGeneratedException(expandee, realex)
              case fatal => throw fatal
            }
        } finally {
          expandee.removeAttachment[MacroRuntimeAttachment]
        }
    }
  }

  /** Expands a macro when a runtime (i.e. the macro implementation) cannot be loaded
   *  Meant for internal use within the macro infrastructure, don't use it elsewhere.
   */
  def macroExpandWithoutRuntime(typer: Typer, expandee: Tree): MacroStatus = {
    import typer.TyperErrorGen._
    val fallbackSym = expandee.symbol.nextOverriddenSymbol orElse MacroImplementationNotFoundError(expandee)
    macroLogLite(s"falling back to: $fallbackSym")

    def mkFallbackTree(tree: Tree): Tree = tree match {
      case Select(qual, name)  => Select(qual, name) setPos tree.pos setSymbol fallbackSym
      case Apply(fn, args)     => Apply(mkFallbackTree(fn), args) setPos tree.pos
      case TypeApply(fn, args) => TypeApply(mkFallbackTree(fn), args) setPos tree.pos
      case x                   => throw new MatchError(x)
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
  var hasPendingMacroExpansions = false // JZ this is never reset to false. What is its purpose? Should it not be stored in Context?
  def typerShouldExpandDeferredMacros: Boolean = hasPendingMacroExpansions && !delayed.isEmpty
  private val forced = perRunCaches.newWeakSet[Tree]()
  private val delayed = perRunCaches.newWeakMap[Tree, scala.collection.mutable.Set[Symbol]]()
  private def isDelayed(expandee: Tree) = !delayed.isEmpty && (delayed contains expandee)
  def clearDelayed(): Unit = delayed.clear()
  private def calculateUndetparams(expandee: Tree): scala.collection.mutable.Set[Symbol] =
    if (forced(expandee)) scala.collection.mutable.Set[Symbol]()
    else delayed.getOrElse(expandee, {
      val calculated = scala.collection.mutable.Set[Symbol]()
      expandee foreach (sub => {
        def traverse(sym: Symbol) = if (sym != null && (undetparams contains sym)) calculated += sym
        if (sub.symbol != null) traverse(sub.symbol)
        if (sub.tpe != null) sub.tpe foreach (sub => traverse(sub.typeSymbol))
      })
      macroLogVerbose("calculateUndetparams: %s".format(calculated))
      calculated
    })
  private val undetparams = perRunCaches.newSet[Symbol]()
  def notifyUndetparamsAdded(newUndets: List[Symbol]): Unit = {
    undetparams ++= newUndets
    if (macroDebugVerbose) newUndets foreach (sym => println("undetParam added: %s".format(sym)))
  }
  def notifyUndetparamsInferred(undetNoMore: List[Symbol], inferreds: List[Type]): Unit = {
    undetparams --= undetNoMore
    if (macroDebugVerbose) (undetNoMore zip inferreds) foreach { case (sym, tpe) => println("undetParam inferred: %s as %s".format(sym, tpe))}
    if (!delayed.isEmpty)
      delayed.toList foreach {
        case (expandee, undetparams) if !undetparams.isEmpty =>
          undetparams --= undetNoMore
          if (undetparams.isEmpty) {
            hasPendingMacroExpansions = true
            macroLogVerbose(s"macro expansion is pending: $expandee")
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
    new AstTransformer {
      override def transform(tree: Tree) = super.transform(tree match {
        // todo. expansion should work from the inside out
        case tree if (delayed contains tree) && calculateUndetparams(tree).isEmpty && !tree.isErroneous =>
          val context = tree.attachments.get[MacroRuntimeAttachment].get.typerContext
          delayed -= tree
          context.implicitsEnabled = typer.context.implicitsEnabled
          context.enrichmentEnabled = typer.context.enrichmentEnabled
          context.macrosEnabled = typer.context.macrosEnabled
          try {
            macroExpand(newTyper(context), tree, EXPRmode, WildcardType)
          } finally {
            if (context.reporter.isBuffering)
              context.reporter.propagateErrorsTo(typer.context.reporter)
          }
        case _ =>
          tree
      })
    }.transform(expandee)
}

object Macros {
  final val macroClassLoadersCache =
    new scala.tools.nsc.classpath.FileBasedCache[Unit, ScalaClassLoader.URLClassLoader]()
}

trait MacrosStats {
  self: TypesStats with Statistics =>
  val macroExpandCount    = newCounter ("#macro expansions", "typer")
  val macroExpandNanos    = newSubTimer("time spent in macroExpand", typerNanos)
}

class Fingerprint private[Fingerprint](val value: Int) extends AnyVal {
  def paramPos = { assert(isTag, this); value }
  def isTag = value >= 0
  override def toString = this match {
    case Other => "Other"
    case LiftedTyped => "Expr"
    case LiftedUntyped => "Tree"
    case _ => s"Tag($value)"
  }
}

object Fingerprint {
  def apply(value: Int) = new Fingerprint(value)
  def Tagged(tparamPos: Int) = new Fingerprint(tparamPos)
  val Other = new Fingerprint(-1)
  val LiftedTyped = new Fingerprint(-2)
  val LiftedUntyped = new Fingerprint(-3)
}
