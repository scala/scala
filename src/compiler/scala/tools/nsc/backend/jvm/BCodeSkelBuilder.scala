/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package jvm

import scala.collection.{ mutable, immutable }
import scala.tools.nsc.backend.jvm.opt.ByteCodeRepository
import scala.tools.nsc.symtab._

import scala.tools.asm
import GenBCode._
import BackendReporting._

/*
 *
 *  @author  Miguel Garcia, http://lamp.epfl.ch/~magarcia/ScalaCompilerCornerReloaded/
 *  @version 1.0
 *
 */
abstract class BCodeSkelBuilder extends BCodeHelpers {
  import global._
  import bTypes._
  import coreBTypes._
  import bCodeAsmCommon._

  /*
   * There's a dedicated PlainClassBuilder for each CompilationUnit,
   * which simplifies the initialization of per-class data structures in `genPlainClass()` which in turn delegates to `initJClass()`
   *
   * The entry-point to emitting bytecode instructions is `genDefDef()` where the per-method data structures are initialized,
   * including `resetMethodBookkeeping()` and `initJMethod()`.
   * Once that's been done, and assuming the method being visited isn't abstract, `emitNormalMethodBody()` populates
   * the ASM MethodNode instance with ASM AbstractInsnNodes.
   *
   * Given that CleanUp delivers trees that produce values on the stack,
   * the entry-point to all-things instruction-emit is `genLoad()`.
   * There, an operation taking N arguments results in recursively emitting instructions to lead each of them,
   * followed by emitting instructions to process those arguments (to be found at run-time on the operand-stack).
   *
   * In a few cases the above recipe deserves more details, as provided in the documentation for:
   *   - `genLoadTry()`
   *   - `genSynchronized()
   *   - `jumpDest` , `cleanups` , `labelDefsAtOrUnder`
   */
  abstract class PlainSkelBuilder(cunit: CompilationUnit)
    extends BCClassGen
    with    BCAnnotGen
    with    BCInnerClassGen
    with    JAndroidBuilder
    with    BCForwardersGen
    with    BCPickles
    with    BCJGenSigGen {

    // Strangely I can't find this in the asm code 255, but reserving 1 for "this"
    final val MaximumJvmParameters = 254

    // current class
    var cnode: asm.tree.ClassNode  = null
    var thisName: String           = null // the internal name of the class being emitted

    var claszSymbol: Symbol        = null
    var isCZParcelable             = false
    var isCZStaticModule           = false
    var isCZRemote                 = false

    protected val indyLambdaHosts = collection.mutable.Set[Symbol]()

    /* ---------------- idiomatic way to ask questions to typer ---------------- */

    def paramTKs(app: Apply): List[BType] = {
      val Apply(fun, _)  = app
      val funSym = fun.symbol
      (funSym.info.paramTypes map toTypeKind) // this tracks mentioned inner classes (in innerClassBufferASM)
    }

    def symInfoTK(sym: Symbol): BType = {
      toTypeKind(sym.info) // this tracks mentioned inner classes (in innerClassBufferASM)
    }

    def tpeTK(tree: Tree): BType = { toTypeKind(tree.tpe) }

    def log(msg: => AnyRef) {
      global synchronized { global.log(msg) }
    }

    override def getCurrentCUnit(): CompilationUnit = { cunit }

    /* ---------------- helper utils for generating classes and fields ---------------- */

    def genPlainClass(cd: ClassDef) {
      assert(cnode == null, "GenBCode detected nested methods.")
      innerClassBufferASM.clear()

      claszSymbol       = cd.symbol
      isCZParcelable    = isAndroidParcelableClass(claszSymbol)
      isCZStaticModule  = isStaticModuleClass(claszSymbol)
      isCZRemote        = isRemote(claszSymbol)
      thisName          = internalName(claszSymbol)

      val classBType = classBTypeFromSymbol(claszSymbol)

      cnode = new asm.tree.ClassNode()

      initJClass(cnode)

      val hasStaticCtor = methodSymbols(cd) exists (_.isStaticConstructor)
      if (!hasStaticCtor) {
        // but needs one ...
        if (isCZStaticModule || isCZParcelable) {
          fabricateStaticInit()
        }
      }

      val optSerial: Option[Long] = serialVUID(claszSymbol)
      if (optSerial.isDefined) { addSerialVUID(optSerial.get, cnode)}

      addClassFields()

      innerClassBufferASM ++= classBType.info.get.nestedClasses
      gen(cd.impl)


      val shouldAddLambdaDeserialize = (
        settings.target.value == "jvm-1.8"
          && settings.Ydelambdafy.value == "method"
          && indyLambdaHosts.contains(claszSymbol))

      if (shouldAddLambdaDeserialize)
        addLambdaDeserialize(claszSymbol, cnode)

      addInnerClassesASM(cnode, innerClassBufferASM.toList)

      cnode.visitAttribute(classBType.inlineInfoAttribute.get)

      if (AsmUtils.traceClassEnabled && cnode.name.contains(AsmUtils.traceClassPattern))
        AsmUtils.traceClass(cnode)

      if (settings.YoptAddToBytecodeRepository) {
        // The inliner needs to find all classes in the code repo, also those being compiled
        byteCodeRepository.add(cnode, ByteCodeRepository.CompilationUnit)
      }

      assert(cd.symbol == claszSymbol, "Someone messed up BCodePhase.claszSymbol during genPlainClass().")
    } // end of method genPlainClass()

    /*
     * must-single-thread
     */
    private def initJClass(jclass: asm.ClassVisitor) {

      val bType = classBTypeFromSymbol(claszSymbol)
      val superClass = bType.info.get.superClass.getOrElse(ObjectReference).internalName
      val interfaceNames = bType.info.get.interfaces map {
        case classBType =>
          if (classBType.isNestedClass.get) { innerClassBufferASM += classBType }
          classBType.internalName
      }

      val flags = javaFlags(claszSymbol)

      val thisSignature = getGenericSignature(claszSymbol, claszSymbol.owner)
      cnode.visit(classfileVersion, flags,
                  thisName, thisSignature,
                  superClass, interfaceNames.toArray)

      if (emitSource) {
        cnode.visitSource(cunit.source.toString, null /* SourceDebugExtension */)
      }

      enclosingMethodAttribute(claszSymbol, internalName, asmMethodType(_).descriptor) match {
        case Some(EnclosingMethodEntry(className, methodName, methodDescriptor)) =>
          cnode.visitOuterClass(className, methodName, methodDescriptor)
        case _ => ()
      }

      val ssa = getAnnotPickle(thisName, claszSymbol)
      cnode.visitAttribute(if (ssa.isDefined) pickleMarkerLocal else pickleMarkerForeign)
      emitAnnotations(cnode, claszSymbol.annotations ++ ssa)

      if (isCZStaticModule || isCZParcelable) {

        if (isCZStaticModule) { addModuleInstanceField() }

      } else {

        val skipStaticForwarders = (claszSymbol.isInterface || settings.noForwarders)
        if (!skipStaticForwarders) {
          val lmoc = claszSymbol.companionModule
          // add static forwarders if there are no name conflicts; see bugs #363 and #1735
          if (lmoc != NoSymbol) {
            // it must be a top level class (name contains no $s)
            val isCandidateForForwarders = {
              exitingPickler { !(lmoc.name.toString contains '$') && lmoc.hasModuleFlag && !lmoc.isImplClass && !lmoc.isNestedClass }
            }
            if (isCandidateForForwarders) {
              log(s"Adding static forwarders from '$claszSymbol' to implementations in '$lmoc'")
              addForwarders(isRemote(claszSymbol), cnode, thisName, lmoc.moduleClass)
            }
          }
        }

      }

      // the invoker is responsible for adding a class-static constructor.

    } // end of method initJClass

    /*
     * can-multi-thread
     */
    private def addModuleInstanceField() {
      val fv =
        cnode.visitField(GenBCode.PublicStaticFinal, // TODO confirm whether we really don't want ACC_SYNTHETIC nor ACC_DEPRECATED
                         strMODULE_INSTANCE_FIELD,
                         "L" + thisName + ";",
                         null, // no java-generic-signature
                         null  // no initial value
        )

      fv.visitEnd()
    }

    /*
     * must-single-thread
     */
    private def fabricateStaticInit() {

      val clinit: asm.MethodVisitor = cnode.visitMethod(
        GenBCode.PublicStatic, // TODO confirm whether we really don't want ACC_SYNTHETIC nor ACC_DEPRECATED
        CLASS_CONSTRUCTOR_NAME,
        "()V",
        null, // no java-generic-signature
        null  // no throwable exceptions
      )
      clinit.visitCode()

      /* "legacy static initialization" */
      if (isCZStaticModule) {
        clinit.visitTypeInsn(asm.Opcodes.NEW, thisName)
        clinit.visitMethodInsn(asm.Opcodes.INVOKESPECIAL,
                               thisName, INSTANCE_CONSTRUCTOR_NAME, "()V", false)
      }
      if (isCZParcelable) { legacyAddCreatorCode(clinit, cnode, thisName) }
      clinit.visitInsn(asm.Opcodes.RETURN)

      clinit.visitMaxs(0, 0) // just to follow protocol, dummy arguments
      clinit.visitEnd()
    }

    def addClassFields() {
      /*  Non-method term members are fields, except for module members. Module
       *  members can only happen on .NET (no flatten) for inner traits. There,
       *  a module symbol is generated (transformInfo in mixin) which is used
       *  as owner for the members of the implementation class (so that the
       *  backend emits them as static).
       *  No code is needed for this module symbol.
       */
      for (f <- fieldSymbols(claszSymbol)) {
        val javagensig = getGenericSignature(f, claszSymbol)
        val flags = javaFieldFlags(f)

        val jfield = new asm.tree.FieldNode(
          flags,
          f.javaSimpleName.toString,
          symInfoTK(f).descriptor,
          javagensig,
          null // no initial value
        )
        cnode.fields.add(jfield)
        emitAnnotations(jfield, f.annotations)
      }

    } // end of method addClassFields()

    // current method
    var mnode: asm.tree.MethodNode = null
    var jMethodName: String        = null
    var isMethSymStaticCtor        = false
    var returnType: BType          = null
    var methSymbol: Symbol         = null
    // in GenASM this is local to genCode(), ie should get false whenever a new method is emitted (including fabricated ones eg addStaticInit())
    var isModuleInitialized        = false
    // used by genLoadTry() and genSynchronized()
    var earlyReturnVar: Symbol     = null
    var shouldEmitCleanup          = false
    var insideCleanupBlock         = false
    // line numbers
    var lastEmittedLineNr          = -1

    object bc extends JCodeMethodN {
      override def jmethod = PlainSkelBuilder.this.mnode
    }

    /* ---------------- Part 1 of program points, ie Labels in the ASM world ---------------- */

    /*
     *  A jump is represented as an Apply node whose symbol denotes a LabelDef, the target of the jump.
     *  The `jumpDest` map is used to:
     *    (a) find the asm.Label for the target, given an Apply node's symbol;
     *    (b) anchor an asm.Label in the instruction stream, given a LabelDef node.
     *  In other words, (a) is necessary when visiting a jump-source, and (b) when visiting a jump-target.
     *  A related map is `labelDef`: it has the same keys as `jumpDest` but its values are LabelDef nodes not asm.Labels.
     *
     */
    var jumpDest: immutable.Map[ /* LabelDef */ Symbol, asm.Label ] = null
    def programPoint(labelSym: Symbol): asm.Label = {
      assert(labelSym.isLabel, s"trying to map a non-label symbol to an asm.Label, at: ${labelSym.pos}")
      jumpDest.getOrElse(labelSym, {
        val pp = new asm.Label
        jumpDest += (labelSym -> pp)
        pp
      })
    }

    /*
     *  A program point may be lexically nested (at some depth)
     *    (a) in the try-clause of a try-with-finally expression
     *    (b) in a synchronized block.
     *  Each of the constructs above establishes a "cleanup block" to execute upon
     *  both normal-exit, early-return, and abrupt-termination of the instructions it encloses.
     *
     *  The `cleanups` LIFO queue represents the nesting of active (for the current program point)
     *  pending cleanups. For each such cleanup an asm.Label indicates the start of its cleanup-block.
     *  At any given time during traversal of the method body,
     *  the head of `cleanups` denotes the cleanup-block for the closest enclosing try-with-finally or synchronized-expression.
     *
     *  `cleanups` is used:
     *
     *    (1) upon visiting a Return statement.
     *        In case of pending cleanups, we can't just emit a RETURN instruction, but must instead:
     *          - store the result (if any) in `earlyReturnVar`, and
     *          - jump to the next pending cleanup.
     *        See `genReturn()`
     *
     *    (2) upon emitting a try-with-finally or a synchronized-expr,
     *        In these cases, the targets of the above jumps are emitted,
     *        provided an early exit was actually encountered somewhere in the protected clauses.
     *        See `genLoadTry()` and `genSynchronized()`
     *
     *  The code thus emitted for jumps and targets covers the early-return case.
     *  The case of abrupt (ie exceptional) termination is covered by exception handlers
     *  emitted for that purpose as described in `genLoadTry()` and `genSynchronized()`.
     */
    var cleanups: List[asm.Label] = Nil
    def registerCleanup(finCleanup: asm.Label) {
      if (finCleanup != null) { cleanups = finCleanup :: cleanups }
    }
    def unregisterCleanup(finCleanup: asm.Label) {
      if (finCleanup != null) {
        assert(cleanups.head eq finCleanup,
               s"Bad nesting of cleanup operations: $cleanups trying to unregister: $finCleanup")
        cleanups = cleanups.tail
      }
    }

    /* ---------------- local variables and params ---------------- */

    case class Local(tk: BType, name: String, idx: Int, isSynth: Boolean)

    /*
     * Bookkeeping for method-local vars and method-params.
     *
     * TODO: use fewer slots. local variable slots are never re-used in separate blocks.
     * In the following example, x and y could use the same slot.
     *   def foo() = {
     *     { val x = 1 }
     *     { val y = "a" }
     *   }
     */
    object locals {

      private val slots = mutable.Map.empty[Symbol, Local] // (local-or-param-sym -> Local(BType, name, idx, isSynth))

      private var nxtIdx = -1 // next available index for local-var

      def reset(isStaticMethod: Boolean) {
        slots.clear()
        nxtIdx = if (isStaticMethod) 0 else 1
      }

      def contains(locSym: Symbol): Boolean = { slots.contains(locSym) }

      def apply(locSym: Symbol): Local = { slots.apply(locSym) }

      /* Make a fresh local variable, ensuring a unique name.
       * The invoker must make sure inner classes are tracked for the sym's tpe.
       */
      def makeLocal(tk: BType, name: String): Symbol = {
        val locSym = methSymbol.newVariable(cunit.freshTermName(name), NoPosition, Flags.SYNTHETIC) // setInfo tpe
        makeLocal(locSym, tk)
        locSym
      }

      def makeLocal(locSym: Symbol): Local = {
        makeLocal(locSym, symInfoTK(locSym))
      }

      def getOrMakeLocal(locSym: Symbol): Local = {
        // `getOrElse` below has the same effect as `getOrElseUpdate` because `makeLocal()` adds an entry to the `locals` map.
        slots.getOrElse(locSym, makeLocal(locSym))
      }

      private def makeLocal(sym: Symbol, tk: BType): Local = {
        assert(!slots.contains(sym), "attempt to create duplicate local var.")
        assert(nxtIdx != -1, "not a valid start index")
        val loc = Local(tk, sym.javaSimpleName.toString, nxtIdx, sym.isSynthetic)
        slots += (sym -> loc)
        assert(tk.size > 0, "makeLocal called for a symbol whose type is Unit.")
        nxtIdx += tk.size
        loc
      }

      // not to be confused with `fieldStore` and `fieldLoad` which also take a symbol but a field-symbol.
      def store(locSym: Symbol) {
        val Local(tk, _, idx, _) = slots(locSym)
        bc.store(idx, tk)
      }

      def load(locSym: Symbol) {
        val Local(tk, _, idx, _) = slots(locSym)
        bc.load(idx, tk)
      }

    }

    /* ---------------- Part 2 of program points, ie Labels in the ASM world ---------------- */

    /*
     *  The semantics of try-with-finally and synchronized-expr require their cleanup code
     *  to be present in three forms in the emitted bytecode:
     *    (a) as normal-exit code, reached via fall-through from the last program point being protected,
     *    (b) as code reached upon early-return from an enclosed return statement.
     *        The only difference between (a) and (b) is their next program-point:
     *          the former must continue with fall-through while
     *          the latter must continue to the next early-return cleanup (if any, otherwise return from the method).
     *        Otherwise they are identical.
     *    (c) as exception-handler, reached via exceptional control flow,
     *        which rethrows the caught exception once it's done with the cleanup code.
     *
     *  A particular cleanup may in general contain LabelDefs. Care is needed when duplicating such jump-targets,
     *  so as to preserve agreement with the (also duplicated) jump-sources.
     *  This is achieved based on the bookkeeping provided by two maps:
     *    - `labelDefsAtOrUnder` lists all LabelDefs enclosed by a given Tree node (the key)
     *    - `labelDef` provides the LabelDef node whose symbol is used as key.
     *       As a sidenote, a related map is `jumpDest`: it has the same keys as `labelDef` but its values are asm.Labels not LabelDef nodes.
     *
     *  Details in `emitFinalizer()`, which is invoked from `genLoadTry()` and `genSynchronized()`.
     */
    var labelDefsAtOrUnder: scala.collection.Map[Tree, List[LabelDef]] = null
    var labelDef: scala.collection.Map[Symbol, LabelDef] = null// (LabelDef-sym -> LabelDef)

    // bookkeeping the scopes of non-synthetic local vars, to emit debug info (`emitVars`).
    var varsInScope: List[Tuple2[Symbol, asm.Label]] = null // (local-var-sym -> start-of-scope)

    // helpers around program-points.
    def lastInsn: asm.tree.AbstractInsnNode = {
      mnode.instructions.getLast
    }
    def currProgramPoint(): asm.Label = {
      lastInsn match {
        case labnode: asm.tree.LabelNode => labnode.getLabel
        case _ =>
          val pp = new asm.Label
          mnode visitLabel pp
          pp
      }
    }
    def markProgramPoint(lbl: asm.Label) {
      val skip = (lbl == null) || isAtProgramPoint(lbl)
      if (!skip) { mnode visitLabel lbl }
    }
    def isAtProgramPoint(lbl: asm.Label): Boolean = {
      (lastInsn match { case labnode: asm.tree.LabelNode => (labnode.getLabel == lbl); case _ => false } )
    }
    def lineNumber(tree: Tree) {
      if (!emitLines || !tree.pos.isDefined) return;
      val nr = tree.pos.finalPosition.line
      if (nr != lastEmittedLineNr) {
        lastEmittedLineNr = nr
        lastInsn match {
          case lnn: asm.tree.LineNumberNode =>
            // overwrite previous landmark as no instructions have been emitted for it
            lnn.line = nr
          case _ =>
            mnode.visitLineNumber(nr, currProgramPoint())
        }
      }
    }

    // on entering a method
    def resetMethodBookkeeping(dd: DefDef) {
      locals.reset(isStaticMethod = methSymbol.isStaticMember)
      jumpDest = immutable.Map.empty[ /* LabelDef */ Symbol, asm.Label ]
      // populate labelDefsAtOrUnder
      val ldf = new LabelDefsFinder
      ldf.traverse(dd.rhs)
      labelDefsAtOrUnder = ldf.result.withDefaultValue(Nil)
      labelDef = labelDefsAtOrUnder(dd.rhs).map(ld => (ld.symbol -> ld)).toMap
      // check previous invocation of genDefDef exited as many varsInScope as it entered.
      assert(varsInScope == null, "Unbalanced entering/exiting of GenBCode's genBlock().")
      // check previous invocation of genDefDef unregistered as many cleanups as it registered.
      assert(cleanups == Nil, "Previous invocation of genDefDef didn't unregister as many cleanups as it registered.")
      isModuleInitialized = false
      earlyReturnVar      = null
      shouldEmitCleanup   = false

      lastEmittedLineNr = -1
    }

    /* ---------------- top-down traversal invoking ASM Tree API along the way ---------------- */

    def gen(tree: Tree) {
      tree match {
        case EmptyTree => ()

        case _: ModuleDef => abort(s"Modules should have been eliminated by refchecks: $tree")

        case ValDef(mods, name, tpt, rhs) => () // fields are added in `genPlainClass()`, via `addClassFields()`

        case dd : DefDef => genDefDef(dd)

        case Template(_, _, body) => body foreach gen

        case _ => abort(s"Illegal tree in gen: $tree")
      }
    }

    /*
     * must-single-thread
     */
    def initJMethod(flags: Int, paramAnnotations: List[List[AnnotationInfo]]) {

      val jgensig = getGenericSignature(methSymbol, claszSymbol)
      addRemoteExceptionAnnot(isCZRemote, hasPublicBitSet(flags), methSymbol)
      val (excs, others) = methSymbol.annotations partition (_.symbol == definitions.ThrowsClass)
      val thrownExceptions: List[String] = getExceptions(excs)

      val bytecodeName =
        if (isMethSymStaticCtor) CLASS_CONSTRUCTOR_NAME
        else jMethodName

      val mdesc = asmMethodType(methSymbol).descriptor
      mnode = cnode.visitMethod(
        flags,
        bytecodeName,
        mdesc,
        jgensig,
        mkArray(thrownExceptions)
      ).asInstanceOf[asm.tree.MethodNode]

      // TODO param names: (m.params map (p => javaName(p.sym)))

      emitAnnotations(mnode, others)
      emitParamAnnotations(mnode, paramAnnotations)

    } // end of method initJMethod


    def genDefDef(dd: DefDef) {
      // the only method whose implementation is not emitted: getClass()
      if (definitions.isGetClass(dd.symbol)) { return }
      assert(mnode == null, "GenBCode detected nested method.")

      methSymbol  = dd.symbol
      jMethodName = methSymbol.javaSimpleName.toString
      returnType  = asmMethodType(dd.symbol).returnType
      isMethSymStaticCtor = methSymbol.isStaticConstructor

      resetMethodBookkeeping(dd)

      // add method-local vars for params
      val DefDef(_, _, _, vparamss, _, rhs) = dd
      assert(vparamss.isEmpty || vparamss.tail.isEmpty, s"Malformed parameter list: $vparamss")
      val params = if (vparamss.isEmpty) Nil else vparamss.head
      for (p <- params) { locals.makeLocal(p.symbol) }
      // debug assert((params.map(p => locals(p.symbol).tk)) == asmMethodType(methSymbol).getArgumentTypes.toList, "debug")

      if (params.size > MaximumJvmParameters) {
        // SI-7324
        reporter.error(methSymbol.pos, s"Platform restriction: a parameter list's length cannot exceed $MaximumJvmParameters.")
        return
      }

      val isNative         = methSymbol.hasAnnotation(definitions.NativeAttr)
      val isAbstractMethod = (methSymbol.isDeferred || methSymbol.owner.isInterface)
      val flags = GenBCode.mkFlags(
        javaFlags(methSymbol),
        if (claszSymbol.isInterface) asm.Opcodes.ACC_ABSTRACT   else 0,
        if (methSymbol.isStrictFP)   asm.Opcodes.ACC_STRICT     else 0,
        if (isNative)                asm.Opcodes.ACC_NATIVE     else 0  // native methods of objects are generated in mirror classes
      )

      // TODO needed? for(ann <- m.symbol.annotations) { ann.symbol.initialize }
      initJMethod(flags, params.map(p => p.symbol.annotations))

      /* Add method-local vars for LabelDef-params.
       *
       * This makes sure that:
       *   (1) upon visiting any "forward-jumping" Apply (ie visited before its target LabelDef), and after
       *   (2) grabbing the corresponding param symbols,
       * those param-symbols can be used to access method-local vars.
       *
       * When duplicating a finally-contained LabelDef, another program-point is needed for the copy (each such copy has its own asm.Label),
       * but the same vars (given by the LabelDef's params) can be reused,
       * because no LabelDef ends up nested within itself after such duplication.
       */
      for(ld <- labelDefsAtOrUnder(dd.rhs); ldp <- ld.params; if !locals.contains(ldp.symbol)) {
        // the tail-calls xform results in symbols shared btw method-params and labelDef-params, thus the guard above.
        locals.makeLocal(ldp.symbol)
      }

      if (!isAbstractMethod && !isNative) {

        def emitNormalMethodBody() {
          val veryFirstProgramPoint = currProgramPoint()
          genLoad(rhs, returnType)

          rhs match {
            case Block(_, Return(_)) => ()
            case Return(_) => ()
            case EmptyTree =>
              globalError("Concrete method has no definition: " + dd + (
                if (settings.debug) "(found: " + methSymbol.owner.info.decls.toList.mkString(", ") + ")"
                else "")
              )
            case _ =>
              bc emitRETURN returnType
          }
          if (emitVars) {
            // add entries to LocalVariableTable JVM attribute
            val onePastLastProgramPoint = currProgramPoint()
            val hasStaticBitSet = ((flags & asm.Opcodes.ACC_STATIC) != 0)
            if (!hasStaticBitSet) {
              mnode.visitLocalVariable(
                "this",
                "L" + thisName + ";",
                null,
                veryFirstProgramPoint,
                onePastLastProgramPoint,
                0
              )
            }
            for (p <- params) { emitLocalVarScope(p.symbol, veryFirstProgramPoint, onePastLastProgramPoint, force = true) }
          }

          if (isMethSymStaticCtor) { appendToStaticCtor(dd) }
        } // end of emitNormalMethodBody()

        lineNumber(rhs)
        emitNormalMethodBody()

        // Note we don't invoke visitMax, thus there are no FrameNode among mnode.instructions.
        // The only non-instruction nodes to be found are LabelNode and LineNumberNode.
      }

      if (AsmUtils.traceMethodEnabled && mnode.name.contains(AsmUtils.traceMethodPattern))
        AsmUtils.traceMethod(mnode)

      mnode = null
    } // end of method genDefDef()

    /*
     *  must-single-thread
     *
     *  TODO document, explain interplay with `fabricateStaticInit()`
     */
    private def appendToStaticCtor(dd: DefDef) {

      def insertBefore(
            location: asm.tree.AbstractInsnNode,
            i0: asm.tree.AbstractInsnNode,
            i1: asm.tree.AbstractInsnNode) {
        if (i0 != null) {
          mnode.instructions.insertBefore(location, i0.clone(null))
          mnode.instructions.insertBefore(location, i1.clone(null))
        }
      }

      // collect all return instructions
      var rets: List[asm.tree.AbstractInsnNode] = Nil
      mnode foreachInsn { i => if (i.getOpcode() == asm.Opcodes.RETURN) { rets ::= i  } }
      if (rets.isEmpty) { return }

      var insnModA: asm.tree.AbstractInsnNode = null
      var insnModB: asm.tree.AbstractInsnNode = null
      // call object's private ctor from static ctor
      if (isCZStaticModule) {
        // NEW `moduleName`
        val className = internalName(methSymbol.enclClass)
        insnModA      = new asm.tree.TypeInsnNode(asm.Opcodes.NEW, className)
        // INVOKESPECIAL <init>
        val callee = methSymbol.enclClass.primaryConstructor
        val jname  = callee.javaSimpleName.toString
        val jowner = internalName(callee.owner)
        val jtype  = asmMethodType(callee).descriptor
        insnModB   = new asm.tree.MethodInsnNode(asm.Opcodes.INVOKESPECIAL, jowner, jname, jtype, false)
      }

      var insnParcA: asm.tree.AbstractInsnNode = null
      var insnParcB: asm.tree.AbstractInsnNode = null
      // android creator code
      if (isCZParcelable) {
        // add a static field ("CREATOR") to this class to cache android.os.Parcelable$Creator
        val andrFieldDescr = getClassBTypeAndRegisterInnerClass(AndroidCreatorClass).descriptor
        cnode.visitField(
          asm.Opcodes.ACC_STATIC | asm.Opcodes.ACC_FINAL,
          "CREATOR",
          andrFieldDescr,
          null,
          null
        )
        // INVOKESTATIC CREATOR(): android.os.Parcelable$Creator; -- TODO where does this Android method come from?
        val callee = definitions.getMember(claszSymbol.companionModule, androidFieldName)
        val jowner = internalName(callee.owner)
        val jname  = callee.javaSimpleName.toString
        val jtype  = asmMethodType(callee).descriptor
        insnParcA  = new asm.tree.MethodInsnNode(asm.Opcodes.INVOKESTATIC, jowner, jname, jtype, false)
        // PUTSTATIC `thisName`.CREATOR;
        insnParcB  = new asm.tree.FieldInsnNode(asm.Opcodes.PUTSTATIC, thisName, "CREATOR", andrFieldDescr)
      }

      // insert a few instructions for initialization before each return instruction
      for(r <- rets) {
        insertBefore(r, insnModA,  insnModB)
        insertBefore(r, insnParcA, insnParcB)
      }

    }

    def emitLocalVarScope(sym: Symbol, start: asm.Label, end: asm.Label, force: Boolean = false) {
      val Local(tk, name, idx, isSynth) = locals(sym)
      if (force || !isSynth) {
        mnode.visitLocalVariable(name, tk.descriptor, null, start, end, idx)
      }
    }

    def genLoad(tree: Tree, expectedType: BType)

  } // end of class PlainSkelBuilder

}
