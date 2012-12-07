/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package icode

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.{ ListBuffer, Buffer }
import scala.tools.nsc.symtab._
import scala.annotation.switch
import PartialFunction._
import scala.language.postfixOps

/** This class ...
 *
 *  @author  Iulian Dragos
 *  @version 1.0
 */
abstract class GenICode extends SubComponent  {
  import global._
  import icodes._
  import icodes.opcodes._
  import definitions.{
    ArrayClass, ObjectClass, ThrowableClass, StringClass, StringModule, AnyRefClass,
    Object_equals, Object_isInstanceOf, Object_asInstanceOf, ScalaRunTimeModule,
    BoxedNumberClass, BoxedCharacterClass,
    getMember
  }
  import scalaPrimitives.{
    isArrayOp, isComparisonOp, isLogicalOp,
    isUniversalEqualityOp, isReferenceEqualityOp
  }
  import platform.isMaybeBoxed

  val phaseName = "icode"

  override def newPhase(prev: Phase) = new ICodePhase(prev)

  @inline private def debugassert(cond: => Boolean, msg: => Any) {
    if (settings.debug.value)
      assert(cond, msg)
  }

  class ICodePhase(prev: Phase) extends StdPhase(prev) {

    override def description = "Generate ICode from the AST"

    var unit: CompilationUnit = NoCompilationUnit

    override def run() {
      scalaPrimitives.init
      classes.clear()
      super.run()
    }

    override def apply(unit: CompilationUnit): Unit = {
      this.unit = unit
      unit.icode.clear
      informProgress("Generating icode for " + unit)
      gen(unit.body)
      this.unit = NoCompilationUnit
    }

    def gen(tree: Tree): Context = gen(tree, new Context())

    def gen(trees: List[Tree], ctx: Context): Context = {
      var ctx1 = ctx
      for (t <- trees) ctx1 = gen(t, ctx1)
      ctx1
    }

    /** If the selector type has a member with the right name,
     *  it is the host class; otherwise the symbol's owner.
     */
    def findHostClass(selector: Type, sym: Symbol) = selector member sym.name match {
      case NoSymbol   => log(s"Rejecting $selector as host class for $sym") ; sym.owner
      case _          => selector.typeSymbol
    }

    /////////////////// Code generation ///////////////////////

    def gen(tree: Tree, ctx: Context): Context = tree match {
      case EmptyTree => ctx

      case PackageDef(pid, stats) =>
        gen(stats, ctx setPackage pid.name)

      case ClassDef(mods, name, _, impl) =>
        debuglog("Generating class: " + tree.symbol.fullName)
        val outerClass = ctx.clazz
        ctx setClass (new IClass(tree.symbol) setCompilationUnit unit)
        addClassFields(ctx, tree.symbol);
        classes += (tree.symbol -> ctx.clazz)
        unit.icode += ctx.clazz
        gen(impl, ctx)
        ctx.clazz.methods = ctx.clazz.methods.reverse // preserve textual order
        ctx.clazz.fields  = ctx.clazz.fields.reverse  // preserve textual order
        ctx setClass outerClass

      // !! modules should be eliminated by refcheck... or not?
      case ModuleDef(mods, name, impl) =>
        abort("Modules should not reach backend! " + tree)

      case ValDef(mods, name, tpt, rhs) =>
        ctx // we use the symbol to add fields

      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        debuglog("Entering method " + name)
        val m = new IMethod(tree.symbol)
        m.sourceFile = unit.source
        m.returnType = if (tree.symbol.isConstructor) UNIT
                       else toTypeKind(tree.symbol.info.resultType)
        ctx.clazz.addMethod(m)

        var ctx1 = ctx.enterMethod(m, tree.asInstanceOf[DefDef])
        addMethodParams(ctx1, vparamss)
        m.native = m.symbol.hasAnnotation(definitions.NativeAttr)

        if (!m.isAbstractMethod && !m.native) {
          ctx1 = genLoad(rhs, ctx1, m.returnType);

          // reverse the order of the local variables, to match the source-order
          m.locals = m.locals.reverse

          rhs match {
            case Block(_, Return(_)) => ()
            case Return(_) => ()
            case EmptyTree =>
              globalError("Concrete method has no definition: " + tree + (
                if (settings.debug.value) "(found: " + m.symbol.owner.info.decls.toList.mkString(", ") + ")"
                else "")
              )
            case _ => if (ctx1.bb.isEmpty)
              ctx1.bb.closeWith(RETURN(m.returnType), rhs.pos)
            else
              ctx1.bb.closeWith(RETURN(m.returnType))
          }
          if (!ctx1.bb.closed) ctx1.bb.close
          prune(ctx1.method)
        } else
          ctx1.method.setCode(NoCode)
        ctx1

      case Template(_, _, body) =>
        gen(body, ctx)

      case _ =>
        abort("Illegal tree in gen: " + tree)
    }

    private def genStat(trees: List[Tree], ctx: Context): Context =
      trees.foldLeft(ctx)((currentCtx, t) => genStat(t, currentCtx))

    /**
     * Generate code for the given tree. The trees should contain statements
     * and not produce any value. Use genLoad for expressions which leave
     * a value on top of the stack.
     *
     * @param tree ...
     * @param ctx  ...
     * @return a new context. This is necessary for control flow instructions
     *         which may change the current basic block.
     */
    private def genStat(tree: Tree, ctx: Context): Context = tree match {
      case Assign(lhs @ Select(_, _), rhs) =>
        val isStatic = lhs.symbol.isStaticMember
        var ctx1 = if (isStatic) ctx
                   else if (forMSIL && msil_IsValuetypeInstField(lhs.symbol))
                     msil_genLoadQualifierAddress(lhs, ctx)
                   else genLoadQualifier(lhs, ctx)

        ctx1 = genLoad(rhs, ctx1, toTypeKind(lhs.symbol.info))
        ctx1.bb.emit(STORE_FIELD(lhs.symbol, isStatic), tree.pos)
        ctx1

      case Assign(lhs, rhs) =>
        val ctx1 = genLoad(rhs, ctx, toTypeKind(lhs.symbol.info))
        val Some(l) = ctx.method.lookupLocal(lhs.symbol)
        ctx1.bb.emit(STORE_LOCAL(l), tree.pos)
        ctx1

      case _ =>
        genLoad(tree, ctx, UNIT)
    }

    private def genThrow(expr: Tree, ctx: Context): (Context, TypeKind) = {
      require(expr.tpe <:< ThrowableClass.tpe, expr.tpe)

      val thrownKind = toTypeKind(expr.tpe)
      val ctx1       = genLoad(expr, ctx, thrownKind)
      ctx1.bb.emit(THROW(expr.tpe.typeSymbol), expr.pos)
      ctx1.bb.enterIgnoreMode

      (ctx1, NothingReference)
    }

    /**
     * Generate code for primitive arithmetic operations.
     * Returns (Context, Generated Type)
     */
    private def genArithmeticOp(tree: Tree, ctx: Context, code: Int): (Context, TypeKind) = {
      val Apply(fun @ Select(larg, _), args) = tree
      var ctx1 = ctx
      var resKind = toTypeKind(larg.tpe)

      debugassert(args.length <= 1,
               "Too many arguments for primitive function: " + fun.symbol)
      debugassert(resKind.isNumericType | resKind == BOOL,
               resKind.toString() + " is not a numeric or boolean type " +
               "[operation: " + fun.symbol + "]")

      args match {
        // unary operation
        case Nil =>
          ctx1 = genLoad(larg, ctx1, resKind)
          code match {
            case scalaPrimitives.POS =>
              () // nothing
            case scalaPrimitives.NEG =>
              ctx1.bb.emit(CALL_PRIMITIVE(Negation(resKind)), larg.pos)
            case scalaPrimitives.NOT =>
              ctx1.bb.emit(CALL_PRIMITIVE(Arithmetic(NOT, resKind)), larg.pos)
            case _ =>
              abort("Unknown unary operation: " + fun.symbol.fullName +
                    " code: " + code)
          }

        // binary operation
        case rarg :: Nil =>
          resKind = getMaxType(larg.tpe :: rarg.tpe :: Nil);
          if (scalaPrimitives.isShiftOp(code) || scalaPrimitives.isBitwiseOp(code))
            assert(resKind.isIntegralType | resKind == BOOL,
                 resKind.toString() + " incompatible with arithmetic modulo operation: " + ctx1);

          ctx1 = genLoad(larg, ctx1, resKind)
          ctx1 = genLoad(rarg,
                         ctx1, // check .NET size of shift arguments!
                         if (scalaPrimitives.isShiftOp(code)) INT else resKind)

          val primitiveOp = code match {
            case scalaPrimitives.ADD    => Arithmetic(ADD, resKind)
            case scalaPrimitives.SUB    => Arithmetic(SUB, resKind)
            case scalaPrimitives.MUL    => Arithmetic(MUL, resKind)
            case scalaPrimitives.DIV    => Arithmetic(DIV, resKind)
            case scalaPrimitives.MOD    => Arithmetic(REM, resKind)
            case scalaPrimitives.OR     => Logical(OR, resKind)
            case scalaPrimitives.XOR    => Logical(XOR, resKind)
            case scalaPrimitives.AND    => Logical(AND, resKind)
            case scalaPrimitives.LSL    => Shift(LSL, resKind)
            case scalaPrimitives.LSR    => Shift(LSR, resKind)
            case scalaPrimitives.ASR    => Shift(ASR, resKind)
            case _                      => abort("Unknown primitive: " + fun.symbol + "[" + code + "]")
          }
          ctx1.bb.emit(CALL_PRIMITIVE(primitiveOp), tree.pos)

        case _ =>
          abort("Too many arguments for primitive function: " + tree)
      }
      (ctx1, resKind)
    }

    /** Generate primitive array operations.
     *
     *  @param tree ...
     *  @param ctx  ...
     *  @param code ...
     *  @return     ...
     */
    private def genArrayOp(tree: Tree, ctx: Context, code: Int, expectedType: TypeKind): (Context, TypeKind) = {
      import scalaPrimitives._
      val Apply(Select(arrayObj, _), args) = tree
      val k = toTypeKind(arrayObj.tpe)
      val ARRAY(elem) = k
      var ctx1 = genLoad(arrayObj, ctx, k)
      val elementType = typeOfArrayOp.getOrElse(code, abort("Unknown operation on arrays: " + tree + " code: " + code))

      var generatedType = expectedType

      if (scalaPrimitives.isArrayGet(code)) {
        // load argument on stack
        debugassert(args.length == 1,
                 "Too many arguments for array get operation: " + tree);
        ctx1 = genLoad(args.head, ctx1, INT)
        generatedType = elem
        ctx1.bb.emit(LOAD_ARRAY_ITEM(elementType), tree.pos)
      }
      else if (scalaPrimitives.isArraySet(code)) {
        debugassert(args.length == 2,
                 "Too many arguments for array set operation: " + tree);
        ctx1 = genLoad(args.head, ctx1, INT)
        ctx1 = genLoad(args.tail.head, ctx1, toTypeKind(args.tail.head.tpe))
        // the following line should really be here, but because of bugs in erasure
        // we pretend we generate whatever type is expected from us.
        //generatedType = UNIT

        ctx1.bb.emit(STORE_ARRAY_ITEM(elementType), tree.pos)
      }
      else {
        generatedType = INT
        ctx1.bb.emit(CALL_PRIMITIVE(ArrayLength(elementType)), tree.pos)
      }

      (ctx1, generatedType)
    }
    private def genSynchronized(tree: Apply, ctx: Context, expectedType: TypeKind): (Context, TypeKind) = {
      val Apply(fun, args) = tree
      val monitor = ctx.makeLocal(tree.pos, ObjectClass.tpe, "monitor")
      var monitorResult: Local = null

      // if the synchronized block returns a result, store it in a local variable. just leaving
      // it on the stack is not valid in MSIL (stack is cleaned when leaving try-blocks)
      val argTpe = args.head.tpe
      val hasResult = expectedType != UNIT
      if (hasResult)
        monitorResult = ctx.makeLocal(tree.pos, argTpe, "monitorResult")

      var ctx1 = genLoadQualifier(fun, ctx)
      ctx1.bb.emit(Seq(
        DUP(ObjectReference),
        STORE_LOCAL(monitor),
        MONITOR_ENTER() setPos tree.pos
      ))
      ctx1.enterSynchronized(monitor)
      debuglog("synchronized block start")

      ctx1 = ctx1.Try(
        bodyCtx => {
          val ctx2 = genLoad(args.head, bodyCtx, expectedType /* toTypeKind(tree.tpe.resultType) */)
          if (hasResult)
            ctx2.bb.emit(STORE_LOCAL(monitorResult))
          ctx2.bb.emit(Seq(
            LOAD_LOCAL(monitor),
            MONITOR_EXIT() setPos tree.pos
          ))
          ctx2
        }, List(
          // tree.tpe / fun.tpe is object, which is no longer true after this transformation
          (ThrowableClass, expectedType, exhCtx => {
            exhCtx.bb.emit(Seq(
              LOAD_LOCAL(monitor),
              MONITOR_EXIT() setPos tree.pos,
              THROW(ThrowableClass)
            ))
            exhCtx.bb.enterIgnoreMode
            exhCtx
          })), EmptyTree, tree)

      debuglog("synchronized block end with block %s closed=%s".format(ctx1.bb, ctx1.bb.closed))
      ctx1.exitSynchronized(monitor)
      if (hasResult)
        ctx1.bb.emit(LOAD_LOCAL(monitorResult))
      (ctx1, expectedType)
    }

    private def genLoadIf(tree: If, ctx: Context, expectedType: TypeKind): (Context, TypeKind) = {
      val If(cond, thenp, elsep) = tree

      var thenCtx = ctx.newBlock
      var elseCtx = ctx.newBlock
      val contCtx = ctx.newBlock

      genCond(cond, ctx, thenCtx, elseCtx)

      val ifKind = toTypeKind(tree.tpe)
      val thenKind = toTypeKind(thenp.tpe)
      val elseKind = if (elsep == EmptyTree) UNIT else toTypeKind(elsep.tpe)

      // we need to drop unneeded results, if one branch gives
      // unit and the other gives something on the stack, because
      // the type of 'if' is scala.Any, and its erasure would be Object.
      // But unboxed units are not Objects...
      def hasUnitBranch = thenKind == UNIT || elseKind == UNIT
      val resKind = if (hasUnitBranch) UNIT else ifKind

      if (hasUnitBranch)
        debuglog("Will drop result from an if branch")

      thenCtx = genLoad(thenp, thenCtx, resKind)
      elseCtx = genLoad(elsep, elseCtx, resKind)

      debugassert(!hasUnitBranch || expectedType == UNIT,
        "I produce UNIT in a context where " + expectedType + " is expected!")

      // alternatives may be already closed by a tail-recursive jump
      thenCtx.bb.closeWith(JUMP(contCtx.bb))
      elseCtx.bb.closeWith(
          if (elsep == EmptyTree) JUMP(contCtx.bb)
          else JUMP(contCtx.bb) setPos tree.pos
        )

      (contCtx, resKind)
    }
    private def genLoadTry(tree: Try, ctx: Context, setGeneratedType: TypeKind => Unit): Context = {
      val Try(block, catches, finalizer) = tree
      val kind = toTypeKind(tree.tpe)

      val caseHandlers =
        for (CaseDef(pat, _, body) <- catches.reverse) yield {
          def genWildcardHandler(sym: Symbol): (Symbol, TypeKind, Context => Context) =
            (sym, kind, ctx => {
              ctx.bb.emit(DROP(REFERENCE(sym))) // drop the loaded exception
              genLoad(body, ctx, kind)
            })

          pat match {
            case Typed(Ident(nme.WILDCARD), tpt)  => genWildcardHandler(tpt.tpe.typeSymbol)
            case Ident(nme.WILDCARD)              => genWildcardHandler(ThrowableClass)
            case Bind(_, _)                       =>
              val exception = ctx.method addLocal new Local(pat.symbol, toTypeKind(pat.symbol.tpe), false) // the exception will be loaded and stored into this local

              (pat.symbol.tpe.typeSymbol, kind, {
                ctx: Context =>
                  ctx.bb.emit(STORE_LOCAL(exception), pat.pos);
                  genLoad(body, ctx, kind);
              })
          }
        }

      ctx.Try(
        bodyCtx => {
          setGeneratedType(kind)
          genLoad(block, bodyCtx, kind)
        },
        caseHandlers,
        finalizer,
        tree)
    }

    private def genPrimitiveOp(tree: Apply, ctx: Context, expectedType: TypeKind): (Context, TypeKind) = {
      val sym = tree.symbol
      val Apply(fun @ Select(receiver, _), args) = tree
      val code = scalaPrimitives.getPrimitive(sym, receiver.tpe)

      if (scalaPrimitives.isArithmeticOp(code))
        genArithmeticOp(tree, ctx, code)
      else if (code == scalaPrimitives.CONCAT)
        (genStringConcat(tree, ctx), StringReference)
      else if (code == scalaPrimitives.HASH)
        (genScalaHash(receiver, ctx), INT)
      else if (isArrayOp(code))
        genArrayOp(tree, ctx, code, expectedType)
      else if (isLogicalOp(code) || isComparisonOp(code)) {
        val trueCtx, falseCtx, afterCtx = ctx.newBlock

        genCond(tree, ctx, trueCtx, falseCtx)
        trueCtx.bb.emitOnly(
          CONSTANT(Constant(true)) setPos tree.pos,
          JUMP(afterCtx.bb)
        )
        falseCtx.bb.emitOnly(
          CONSTANT(Constant(false)) setPos tree.pos,
          JUMP(afterCtx.bb)
        )
        (afterCtx, BOOL)
      }
      else if (code == scalaPrimitives.SYNCHRONIZED)
        genSynchronized(tree, ctx, expectedType)
      else if (scalaPrimitives.isCoercion(code)) {
        val ctx1 = genLoad(receiver, ctx, toTypeKind(receiver.tpe))
        genCoercion(tree, ctx1, code)
        (ctx1, scalaPrimitives.generatedKind(code))
      }
      else abort(
        "Primitive operation not handled yet: " + sym.fullName + "(" +
        fun.symbol.simpleName + ") " + " at: " + (tree.pos)
      )
    }

    /**
     * forMSIL
     */
    private def msil_IsValuetypeInstMethod(msym: Symbol) = (
      loaders.clrTypes.methods get msym exists (mMSIL =>
        mMSIL.IsInstance && mMSIL.DeclaringType.IsValueType
      )
    )
    private def msil_IsValuetypeInstField(fsym: Symbol) = (
      loaders.clrTypes.fields get fsym exists (fMSIL =>
        !fMSIL.IsStatic && fMSIL.DeclaringType.IsValueType
      )
    )

    /**
     * forMSIL: Adds a local var, the emitted code requires one more slot on the stack as on entry
     */
    private def msil_genLoadZeroOfNonEnumValuetype(ctx: Context, kind: TypeKind, pos: Position, leaveAddressOnStackInstead: Boolean) {
      val REFERENCE(clssym) = kind
      assert(loaders.clrTypes.isNonEnumValuetype(clssym), clssym)
      val local = ctx.makeLocal(pos, clssym.tpe, "tmp")
      ctx.method.addLocal(local)
      ctx.bb.emit(CIL_LOAD_LOCAL_ADDRESS(local), pos)
      ctx.bb.emit(CIL_INITOBJ(kind), pos)
      val instr = if (leaveAddressOnStackInstead)
                    CIL_LOAD_LOCAL_ADDRESS(local)
                  else
                    LOAD_LOCAL(local)
      ctx.bb.emit(instr, pos)
    }

    /**
     * forMSIL
     */
    private def msil_genLoadAddressOf(tree: Tree, ctx: Context, expectedType: TypeKind, butRawValueIsAlsoGoodEnough: Boolean): Context = {
      var generatedType = expectedType
      var addressTaken = false
      debuglog("at line: " + (if (tree.pos.isDefined) tree.pos.line else tree.pos))

      var resCtx: Context = tree match {

        // emits CIL_LOAD_FIELD_ADDRESS
        case Select(qualifier, selector) if (!tree.symbol.isModule) =>
          addressTaken = true
          val sym = tree.symbol
          generatedType = toTypeKind(sym.info)

          if (sym.isStaticMember) {
            ctx.bb.emit(CIL_LOAD_FIELD_ADDRESS(sym, true), tree.pos)
            ctx
          } else {
            val ctx1 = genLoadQualifier(tree, ctx)
            ctx1.bb.emit(CIL_LOAD_FIELD_ADDRESS(sym, false), tree.pos)
            ctx1
          }

        // emits CIL_LOAD_LOCAL_ADDRESS
        case Ident(name) if (!tree.symbol.isPackage && !tree.symbol.isModule)=>
          addressTaken = true
          val sym = tree.symbol
          try {
            val Some(l) = ctx.method.lookupLocal(sym)
            ctx.bb.emit(CIL_LOAD_LOCAL_ADDRESS(l), tree.pos)
            generatedType = l.kind // actually, should be "V&" but the callsite is aware of this
          } catch {
            case ex: MatchError =>
              abort("symbol " + sym + " does not exist in " + ctx.method)
          }
          ctx

        // emits CIL_LOAD_ARRAY_ITEM_ADDRESS
        case Apply(fun, args) =>
          if (isPrimitive(fun.symbol)) {

            val sym = tree.symbol
            val Apply(fun @ Select(receiver, _), args) = tree
            val code = scalaPrimitives.getPrimitive(sym, receiver.tpe)

            if (isArrayOp(code)) {
              val arrayObj = receiver
              val k = toTypeKind(arrayObj.tpe)
              val ARRAY(elementType) = k
              if (scalaPrimitives.isArrayGet(code)) {
                var ctx1 = genLoad(arrayObj, ctx, k)
                // load argument on stack
                debugassert(args.length == 1, "Too many arguments for array get operation: " + tree)
                ctx1 = genLoad(args.head, ctx1, INT)
                generatedType = elementType // actually "managed pointer to element type" but the callsite is aware of this
                ctx1.bb.emit(CIL_LOAD_ARRAY_ITEM_ADDRESS(elementType), tree.pos)
                addressTaken = true
                ctx1
              } else null
            } else null
          } else null

        case This(qual) =>
          /* TODO: this case handler is a placeholder for the time when Level 2 support for valuetypes is in place,
             in particular when invoking other methods on this where this is a valuetype value (boxed or not).
             As receiver, a managed pointer is expected, and a plain ldarg.0 achieves just that. */
          addressTaken = true
          genLoad(tree, ctx, expectedType)

        case _ =>
          null /* A method returning ByRef won't pass peverify, so I guess this case handler is dead code.
                  Even if it's not, the code below to handler !addressTaken below. */
      }

      if (!addressTaken) {
        resCtx = genLoad(tree, ctx, expectedType)
        if (!butRawValueIsAlsoGoodEnough) {
          // raw value on stack (must be an intermediate result, e.g. returned by method call), take address
          addressTaken = true
          val boxType = expectedType // toTypeKind(expectedType /* TODO FIXME */)
          resCtx.bb.emit(BOX(boxType), tree.pos)
          resCtx.bb.emit(CIL_UNBOX(boxType), tree.pos)
        }
      }

      // emit conversion
      if (generatedType != expectedType)
        abort("Unexpected tree in msil_genLoadAddressOf: " + tree + " at: " + tree.pos)

      resCtx
    }


    /**
     * Generate code for trees that produce values on the stack
     *
     * @param tree The tree to be translated
     * @param ctx  The current context
     * @param expectedType The type of the value to be generated on top of the
     *                     stack.
     * @return The new context. The only thing that may change is the current
     *         basic block (as the labels map is mutable).
     */
    private def genLoad(tree: Tree, ctx: Context, expectedType: TypeKind): Context = {
      var generatedType = expectedType
      debuglog("at line: " + (if (tree.pos.isDefined) tree.pos.line else tree.pos))

      val resCtx: Context = tree match {
        case LabelDef(name, params, rhs) =>
          def genLoadLabelDef = {
            val ctx1 = ctx.newBlock
            if (nme.isLoopHeaderLabel(name))
              ctx1.bb.loopHeader = true

            ctx1.labels.get(tree.symbol) match {
              case Some(label) =>
                debuglog("Found existing label for " + tree.symbol.fullLocationString)
                label.anchor(ctx1.bb)
                label.patch(ctx.method.code)

              case None =>
                val pair = (tree.symbol -> (new Label(tree.symbol) anchor ctx1.bb setParams (params map (_.symbol))))
                debuglog("Adding label " + tree.symbol.fullLocationString + " in genLoad.")
                ctx1.labels += pair
                ctx.method.addLocals(params map (p => new Local(p.symbol, toTypeKind(p.symbol.info), false)));
            }

            ctx.bb.closeWith(JUMP(ctx1.bb), tree.pos)
            genLoad(rhs, ctx1, expectedType /*toTypeKind(tree.symbol.info.resultType)*/)
          }
          genLoadLabelDef

        case ValDef(_, name, _, rhs) =>
          def genLoadValDef =
            if (name == nme.THIS) {
              debuglog("skipping trivial assign to _$this: " + tree)
              ctx
            } else {
              val sym = tree.symbol
              val local = ctx.method.addLocal(new Local(sym, toTypeKind(sym.info), false))

              if (rhs == EmptyTree) {
                debuglog("Uninitialized variable " + tree + " at: " + (tree.pos));
                ctx.bb.emit(getZeroOf(local.kind))
              }

              var ctx1 = ctx
              if (rhs != EmptyTree)
                ctx1 = genLoad(rhs, ctx, local.kind);

              ctx1.bb.emit(STORE_LOCAL(local), tree.pos)
              ctx1.scope.add(local)
              ctx1.bb.emit(SCOPE_ENTER(local))
              generatedType = UNIT
              ctx1
            }
          genLoadValDef

        case t @ If(cond, thenp, elsep) =>
          val (newCtx, resKind) = genLoadIf(t, ctx, expectedType)
          generatedType = resKind
          newCtx

        case Return(expr) =>
          def genLoadReturn = {
            val returnedKind = toTypeKind(expr.tpe)
            debuglog("Return(" + expr + ") with returnedKind = " + returnedKind)

            var ctx1         = genLoad(expr, ctx, returnedKind)
            lazy val tmp     = ctx1.makeLocal(tree.pos, expr.tpe, "tmp")
            val saved        = savingCleanups(ctx1) {
              var savedFinalizer = false
              ctx1.cleanups foreach {
                case MonitorRelease(m) =>
                  debuglog("removing " + m + " from cleanups: " + ctx1.cleanups)
                  ctx1.bb.emit(Seq(LOAD_LOCAL(m), MONITOR_EXIT()))
                  ctx1.exitSynchronized(m)

                case Finalizer(f, finalizerCtx) =>
                  debuglog("removing " + f + " from cleanups: " + ctx1.cleanups)
                  if (returnedKind != UNIT && mayCleanStack(f)) {
                    log("Emitting STORE_LOCAL for " + tmp + " to save finalizer.")
                    ctx1.bb.emit(STORE_LOCAL(tmp))
                    savedFinalizer = true
                  }

                  // duplicate finalizer (takes care of anchored labels)
                  val f1 = duplicateFinalizer(Set.empty ++ ctx1.labels.keySet, ctx1, f)

                  // we have to run this without the same finalizer in
                  // the list, otherwise infinite recursion happens for
                  // finalizers that contain 'return'
                  val fctx = finalizerCtx.newBlock
                  ctx1.bb.closeWith(JUMP(fctx.bb))
                  ctx1 = genLoad(f1, fctx, UNIT)
              }
              savedFinalizer
            }

            if (saved) {
              log("Emitting LOAD_LOCAL for " + tmp + " after saving finalizer.")
              ctx1.bb.emit(LOAD_LOCAL(tmp))
            }
            adapt(returnedKind, ctx1.method.returnType, ctx1, tree.pos)
            ctx1.bb.emit(RETURN(ctx.method.returnType), tree.pos)
            ctx1.bb.enterIgnoreMode
            generatedType = expectedType
            ctx1
          }
          genLoadReturn

        case t @ Try(_, _, _) =>
          genLoadTry(t, ctx, generatedType = _)

        case Throw(expr) =>
          val (ctx1, expectedType) = genThrow(expr, ctx)
          generatedType = expectedType
          ctx1

        case New(tpt) =>
          abort("Unexpected New(" + tpt.summaryString + "/" + tpt + ") received in icode.\n" +
            "  Call was genLoad" + ((tree, ctx, expectedType)))

        case Apply(TypeApply(fun, targs), _) =>
          def genLoadApply1 = {
            val sym = fun.symbol
            val cast = sym match {
              case Object_isInstanceOf  => false
              case Object_asInstanceOf  => true
              case _                    => abort("Unexpected type application " + fun + "[sym: " + sym.fullName + "]" + " in: " + tree)
            }

            val Select(obj, _) = fun
            val l = toTypeKind(obj.tpe)
            val r = toTypeKind(targs.head.tpe)
            val ctx1 = genLoadQualifier(fun, ctx)

            if (l.isValueType && r.isValueType)
              genConversion(l, r, ctx1, cast)
            else if (l.isValueType) {
              ctx1.bb.emit(DROP(l), fun.pos)
              if (cast) {
                ctx1.bb.emit(Seq(
                  NEW(REFERENCE(definitions.ClassCastExceptionClass)),
                  DUP(ObjectReference),
                  THROW(definitions.ClassCastExceptionClass)
                ))
              } else
                ctx1.bb.emit(CONSTANT(Constant(false)))
            } else if (r.isValueType && cast) {
              /* Erasure should have added an unboxing operation to prevent that. */
              abort("should have been unboxed by erasure: " + tree)
            } else if (r.isValueType) {
              ctx.bb.emit(IS_INSTANCE(REFERENCE(definitions.boxedClass(r.toType.typeSymbol))))
            } else {
              genCast(l, r, ctx1, cast)
            }
            generatedType = if (cast) r else BOOL;
            ctx1
          }
          genLoadApply1

        // 'super' call: Note: since constructors are supposed to
        // return an instance of what they construct, we have to take
        // special care. On JVM they are 'void', and Scala forbids (syntactically)
        // to call super constructors explicitly and/or use their 'returned' value.
        // therefore, we can ignore this fact, and generate code that leaves nothing
        // on the stack (contrary to what the type in the AST says).
        case Apply(fun @ Select(Super(_, mix), _), args) =>
          def genLoadApply2 = {
            debuglog("Call to super: " + tree);
            val invokeStyle = SuperCall(mix)
            // if (fun.symbol.isConstructor) Static(true) else SuperCall(mix);

            ctx.bb.emit(THIS(ctx.clazz.symbol), tree.pos)
            val ctx1 = genLoadArguments(args, fun.symbol.info.paramTypes, ctx)

            ctx1.bb.emit(CALL_METHOD(fun.symbol, invokeStyle), tree.pos)
            generatedType =
              if (fun.symbol.isConstructor) UNIT
              else toTypeKind(fun.symbol.info.resultType)
            ctx1
          }
          genLoadApply2

        // 'new' constructor call: Note: since constructors are
        // thought to return an instance of what they construct,
        // we have to 'simulate' it by DUPlicating the freshly created
        // instance (on JVM, <init> methods return VOID).
        case Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) =>
          def genLoadApply3 = {
            val ctor = fun.symbol
            debugassert(ctor.isClassConstructor,
                        "'new' call to non-constructor: " + ctor.name)

            generatedType = toTypeKind(tpt.tpe)
            debugassert(generatedType.isReferenceType || generatedType.isArrayType,
                        "Non reference type cannot be instantiated: " + generatedType)

            generatedType match {
              case arr @ ARRAY(elem) =>
                val ctx1 = genLoadArguments(args, ctor.info.paramTypes, ctx)
                val dims = arr.dimensions
                var elemKind = arr.elementKind
                if (args.length > dims)
                  unit.error(tree.pos, "too many arguments for array constructor: found " + args.length +
                             " but array has only " + dims + " dimension(s)")
                if (args.length != dims)
                  for (i <- args.length until dims) elemKind = ARRAY(elemKind)
                ctx1.bb.emit(CREATE_ARRAY(elemKind, args.length), tree.pos)
                ctx1

              case rt @ REFERENCE(cls) =>
                debugassert(ctor.owner == cls,
                            "Symbol " + ctor.owner.fullName + " is different than " + tpt)

                val ctx2 = if (forMSIL && loaders.clrTypes.isNonEnumValuetype(cls)) {
                  /* parameterful constructors are the only possible custom constructors,
                     a default constructor can't be defined for valuetypes, CLR dixit */
                  val isDefaultConstructor = args.isEmpty
                  if (isDefaultConstructor) {
                    msil_genLoadZeroOfNonEnumValuetype(ctx, rt, tree.pos, leaveAddressOnStackInstead = false)
                    ctx
                  } else {
                    val ctx1 = genLoadArguments(args, ctor.info.paramTypes, ctx)
                    ctx1.bb.emit(CIL_NEWOBJ(ctor), tree.pos)
                    ctx1
                  }
                } else {
                  val nw = NEW(rt)
                  ctx.bb.emit(nw, tree.pos)
                  ctx.bb.emit(DUP(generatedType))
                  val ctx1 = genLoadArguments(args, ctor.info.paramTypes, ctx)

                  val init = CALL_METHOD(ctor, Static(true))
                  nw.init = init
                  ctx1.bb.emit(init, tree.pos)
                  ctx1
                }
                ctx2

              case _ =>
                abort("Cannot instantiate " + tpt + " of kind: " + generatedType)
            }
          }
          genLoadApply3

        case Apply(fun @ _, List(expr)) if (definitions.isBox(fun.symbol)) =>
          def genLoadApply4 = {
            debuglog("BOX : " + fun.symbol.fullName);
            val ctx1 = genLoad(expr, ctx, toTypeKind(expr.tpe))
            val nativeKind = toTypeKind(expr.tpe)
            if (settings.Xdce.value) {
              // we store this boxed value to a local, even if not really needed.
              // boxing optimization might use it, and dead code elimination will
              // take care of unnecessary stores
              var loc1 = ctx.makeLocal(tree.pos, expr.tpe, "boxed")
              ctx1.bb.emit(STORE_LOCAL(loc1))
              ctx1.bb.emit(LOAD_LOCAL(loc1))
            }
            ctx1.bb.emit(BOX(nativeKind), expr.pos)
            generatedType = toTypeKind(fun.symbol.tpe.resultType)
            ctx1
          }
          genLoadApply4

        case Apply(fun @ _, List(expr)) if (definitions.isUnbox(fun.symbol)) =>
          debuglog("UNBOX : " + fun.symbol.fullName)
          val ctx1 = genLoad(expr, ctx, toTypeKind(expr.tpe))
          val boxType = toTypeKind(fun.symbol.owner.linkedClassOfClass.tpe)
          generatedType = boxType
          ctx1.bb.emit(UNBOX(boxType), expr.pos)
          ctx1

        case Apply(fun @ _, List(expr)) if (forMSIL && loaders.clrTypes.isAddressOf(fun.symbol)) =>
          debuglog("ADDRESSOF : " + fun.symbol.fullName);
          val ctx1 = msil_genLoadAddressOf(expr, ctx, toTypeKind(expr.tpe), butRawValueIsAlsoGoodEnough = false)
          generatedType = toTypeKind(fun.symbol.tpe.resultType)
          ctx1

        case app @ Apply(fun, args) =>
          def genLoadApply6 = {
            val sym = fun.symbol

            if (sym.isLabel) {  // jump to a label
              val label = ctx.labels.getOrElse(sym, {
                // it is a forward jump, scan for labels
                resolveForwardLabel(ctx.defdef, ctx, sym)
                ctx.labels.get(sym) match {
                  case Some(l) =>
                    log("Forward jump for " + sym.fullLocationString + ": scan found label " + l)
                    l
                  case _       =>
                    abort("Unknown label target: " + sym + " at: " + (fun.pos) + ": ctx: " + ctx)
                }
              })
              // note: when one of the args to genLoadLabelArguments is a jump to a label,
              // it will call back into genLoad and arrive at this case, which will then set ctx1.bb.ignore to true,
              // this is okay, since we're jumping unconditionally, so the loads and jumps emitted by the outer
              // call to genLoad (by calling genLoadLabelArguments and emitOnly) can safely be ignored,
              // however, as emitOnly will close the block, which reverses its instructions (when it's still open),
              // we better not reverse when the block has already been closed but is in ignore mode
              // (if it's not in ignore mode, double-closing is an error)
              val ctx1 = genLoadLabelArguments(args, label, ctx)
              ctx1.bb.emitOnly(if (label.anchored) JUMP(label.block) else PJUMP(label))
              ctx1.bb.enterIgnoreMode
              ctx1
            } else if (isPrimitive(sym)) { // primitive method call
              val (newCtx, resKind) = genPrimitiveOp(app, ctx, expectedType)
              generatedType = resKind
              newCtx
            } else {  // normal method call
              debuglog("Gen CALL_METHOD with sym: " + sym + " isStaticSymbol: " + sym.isStaticMember);
              val invokeStyle =
                if (sym.isStaticMember)
                  Static(false)
                else if (sym.isPrivate || sym.isClassConstructor)
                  Static(true)
                else
                  Dynamic

              var ctx1 =
                if (invokeStyle.hasInstance) {
                  if (forMSIL && !(invokeStyle.isInstanceOf[SuperCall]) && msil_IsValuetypeInstMethod(sym))
                    msil_genLoadQualifierAddress(fun, ctx)
                  else
                    genLoadQualifier(fun, ctx)
                } else ctx

              ctx1 = genLoadArguments(args, sym.info.paramTypes, ctx1)
              val cm = CALL_METHOD(sym, invokeStyle)

              /** In a couple cases, squirrel away a little extra information in the
               *  CALL_METHOD for use by GenJVM.
               */
              fun match {
                case Select(qual, _) =>
                  val qualSym = findHostClass(qual.tpe, sym)
                  if (qualSym == ArrayClass) {
                    val kind = toTypeKind(qual.tpe)
                    cm setTargetTypeKind kind
                    log(s"Stored target type kind for {$sym.fullName} as $kind")
                  }
                  else {
                    cm setHostClass qualSym
                    if (qual.tpe.typeSymbol != qualSym)
                      log(s"Precisified host class for $sym from ${qual.tpe.typeSymbol.fullName} to ${qualSym.fullName}")
                  }
                case _ =>
              }
              ctx1.bb.emit(cm, tree.pos)

              if (sym == ctx1.method.symbol) {
                ctx1.method.recursive = true
              }
              generatedType =
                if (sym.isClassConstructor) UNIT
                else toTypeKind(sym.info.resultType);
              ctx1
            }
          }
          genLoadApply6

        case ApplyDynamic(qual, args) =>
          assert(!forMSIL, tree)
          // TODO - this is where we'd catch dynamic applies for invokedynamic.
          sys.error("No invokedynamic support yet.")
          // val ctx1 = genLoad(qual, ctx, ObjectReference)
          // genLoadArguments(args, tree.symbol.info.paramTypes, ctx1)
          // ctx1.bb.emit(CALL_METHOD(tree.symbol, InvokeDynamic), tree.pos)
          // ctx1

        case This(qual) =>
          def genLoadThis = {
            assert(tree.symbol == ctx.clazz.symbol || tree.symbol.isModuleClass,
                   "Trying to access the this of another class: " +
                   "tree.symbol = " + tree.symbol + ", ctx.clazz.symbol = " + ctx.clazz.symbol + " compilation unit:"+unit)
            if (tree.symbol.isModuleClass && tree.symbol != ctx.clazz.symbol) {
              genLoadModule(ctx, tree)
              generatedType = REFERENCE(tree.symbol)
            } else {
              ctx.bb.emit(THIS(ctx.clazz.symbol), tree.pos)
              generatedType = REFERENCE(
                if (tree.symbol == ArrayClass) ObjectClass else ctx.clazz.symbol
              )
            }
            ctx
          }
          genLoadThis

        case Select(Ident(nme.EMPTY_PACKAGE_NAME), module) =>
          debugassert(tree.symbol.isModule,
            "Selection of non-module from empty package: " + tree +
            " sym: " + tree.symbol + " at: " + (tree.pos)
          )
          genLoadModule(ctx, tree)

        case Select(qualifier, selector) =>
          def genLoadSelect = {
            val sym = tree.symbol
            generatedType = toTypeKind(sym.info)
            val hostClass = findHostClass(qualifier.tpe, sym)
            log(s"Host class of $sym with qual $qualifier (${qualifier.tpe}) is $hostClass")

            if (sym.isModule) {
              genLoadModule(ctx, tree)
            }
            else if (sym.isStaticMember) {
              ctx.bb.emit(LOAD_FIELD(sym, true) setHostClass hostClass, tree.pos)
              ctx
            } else {
              val ctx1 = genLoadQualifier(tree, ctx)
              ctx1.bb.emit(LOAD_FIELD(sym, false) setHostClass hostClass, tree.pos)
              ctx1
            }
          }
          genLoadSelect

        case Ident(name) =>
          def genLoadIdent = {
            val sym = tree.symbol
            if (!sym.isPackage) {
              if (sym.isModule) {
                genLoadModule(ctx, tree)
                generatedType = toTypeKind(sym.info)
              } else {
                try {
                  val Some(l) = ctx.method.lookupLocal(sym)
                  ctx.bb.emit(LOAD_LOCAL(l), tree.pos)
                  generatedType = l.kind
                } catch {
                  case ex: MatchError =>
                    abort("symbol " + sym + " does not exist in " + ctx.method)
                }
              }
            }
            ctx
          }
          genLoadIdent

        case Literal(value) =>
          def genLoadLiteral = {
            if (value.tag != UnitTag) (value.tag, expectedType) match {
              case (IntTag, LONG) =>
                ctx.bb.emit(CONSTANT(Constant(value.longValue)), tree.pos);
                generatedType = LONG
              case (FloatTag, DOUBLE) =>
                ctx.bb.emit(CONSTANT(Constant(value.doubleValue)), tree.pos);
                generatedType = DOUBLE
              case (NullTag, _) =>
                ctx.bb.emit(CONSTANT(value), tree.pos);
                generatedType = NullReference
              case _ =>
                ctx.bb.emit(CONSTANT(value), tree.pos);
                generatedType = toTypeKind(tree.tpe)
            }
            ctx
          }
          genLoadLiteral

        case Block(stats, expr) =>
          ctx.enterScope
          var ctx1 = genStat(stats, ctx)
          ctx1 = genLoad(expr, ctx1, expectedType)
          ctx1.exitScope
          ctx1

        case Typed(Super(_, _), _) =>
          genLoad(This(ctx.clazz.symbol), ctx, expectedType)

        case Typed(expr, _) =>
          genLoad(expr, ctx, expectedType)

        case Assign(_, _) =>
          generatedType = UNIT
          genStat(tree, ctx)

        case ArrayValue(tpt @ TypeTree(), _elems) =>
          def genLoadArrayValue = {
            var ctx1 = ctx
            val elmKind = toTypeKind(tpt.tpe)
            generatedType = ARRAY(elmKind)
            val elems = _elems.toIndexedSeq

            ctx1.bb.emit(CONSTANT(new Constant(elems.length)), tree.pos)
            ctx1.bb.emit(CREATE_ARRAY(elmKind, 1))
            // inline array literals
            var i = 0
            while (i < elems.length) {
              ctx1.bb.emit(DUP(generatedType), tree.pos)
              ctx1.bb.emit(CONSTANT(new Constant(i)))
              ctx1 = genLoad(elems(i), ctx1, elmKind)
              ctx1.bb.emit(STORE_ARRAY_ITEM(elmKind))
              i = i + 1
            }
            ctx1
          }
          genLoadArrayValue

        case Match(selector, cases) =>
          def genLoadMatch = {
            debuglog("Generating SWITCH statement.");
            var ctx1 = genLoad(selector, ctx, INT) // TODO: Java 7 allows strings in switches (so, don't assume INT and don't convert the literals using intValue)
            val afterCtx = ctx1.newBlock
            var caseCtx: Context  = null
            generatedType = toTypeKind(tree.tpe)

            var targets: List[BasicBlock] = Nil
            var tags: List[Int] = Nil
            var default: BasicBlock = afterCtx.bb

            for (caze @ CaseDef(pat, guard, body) <- cases) {
              assert(guard == EmptyTree, guard)
              val tmpCtx = ctx1.newBlock
              pat match {
                case Literal(value) =>
                  tags = value.intValue :: tags
                  targets = tmpCtx.bb :: targets
                case Ident(nme.WILDCARD) =>
                  default = tmpCtx.bb
                case Alternative(alts) =>
                  alts foreach {
                    case Literal(value) =>
                      tags = value.intValue :: tags
                      targets = tmpCtx.bb :: targets
                    case _ =>
                      abort("Invalid case in alternative in switch-like pattern match: " +
                            tree + " at: " + tree.pos)
                  }
                case _ =>
                  abort("Invalid case statement in switch-like pattern match: " +
                        tree + " at: " + (tree.pos))
              }

              caseCtx = genLoad(body, tmpCtx, generatedType)
              // close the block unless it's already been closed by the body, which closes the block if it ends in a jump (which is emitted to have alternatives share their body)
              caseCtx.bb.closeWith(JUMP(afterCtx.bb) setPos caze.pos)
            }
            ctx1.bb.emitOnly(
              SWITCH(tags.reverse map (x => List(x)), (default :: targets).reverse) setPos tree.pos
            )
            afterCtx
          }
          genLoadMatch

        case EmptyTree =>
          if (expectedType != UNIT)
            ctx.bb.emit(getZeroOf(expectedType))
          ctx

        case _ =>
          abort("Unexpected tree in genLoad: " + tree + "/" + tree.getClass + " at: " + tree.pos)
      }

      // emit conversion
      if (generatedType != expectedType)
        adapt(generatedType, expectedType, resCtx, tree.pos)

      resCtx
    }

    private def adapt(from: TypeKind, to: TypeKind, ctx: Context, pos: Position): Unit = {
      if (!(from <:< to) && !(from == NullReference && to == NothingReference)) {
        to match {
          case UNIT =>
            ctx.bb.emit(DROP(from), pos)
            debuglog("Dropped an " + from);

          case _ =>
            debugassert(from != UNIT, "Can't convert from UNIT to " + to + " at: " + pos)
            assert(!from.isReferenceType && !to.isReferenceType,
              "type error: can't convert from " + from + " to " + to +" in unit " + unit.source + " at " + pos)

            ctx.bb.emit(CALL_PRIMITIVE(Conversion(from, to)), pos)
        }
      } else if (from == NothingReference) {
        ctx.bb.emit(THROW(ThrowableClass))
        ctx.bb.enterIgnoreMode
      } else if (from == NullReference) {
        ctx.bb.emit(DROP(from))
        ctx.bb.emit(CONSTANT(Constant(null)))
      }
      else if (from == ThrowableReference && !(ThrowableClass.tpe <:< to.toType)) {
        log("Inserted check-cast on throwable to " + to + " at " + pos)
        ctx.bb.emit(CHECK_CAST(to))
      }
      else (from, to) match  {
        case (BYTE, LONG) | (SHORT, LONG) | (CHAR, LONG) | (INT, LONG) => ctx.bb.emit(CALL_PRIMITIVE(Conversion(INT, LONG)))
        case _ => ()
      }
    }

    /** Load the qualifier of `tree` on top of the stack. */
    private def genLoadQualifier(tree: Tree, ctx: Context): Context =
      tree match {
        case Select(qualifier, _) =>
          genLoad(qualifier, ctx, toTypeKind(qualifier.tpe))
        case _ =>
          abort("Unknown qualifier " + tree)
      }

    /** forMSIL */
    private def msil_genLoadQualifierAddress(tree: Tree, ctx: Context): Context =
      tree match {
        case Select(qualifier, _) =>
          msil_genLoadAddressOf(qualifier, ctx, toTypeKind(qualifier.tpe), butRawValueIsAlsoGoodEnough = false)
        case _ =>
          abort("Unknown qualifier " + tree)
      }

    /**
     * Generate code that loads args into label parameters.
     */
    private def genLoadLabelArguments(args: List[Tree], label: Label, ctx: Context): Context = {
      debugassert(
        args.length == label.params.length,
        "Wrong number of arguments in call to label " + label.symbol
      )
      var ctx1 = ctx

      def isTrivial(kv: (Tree, Symbol)) = kv match {
        case (This(_), p) if p.name == nme.THIS     => true
        case (arg @ Ident(_), p) if arg.symbol == p => true
        case _                                      => false
      }

      val stores = args zip label.params filterNot isTrivial map {
        case (arg, param) =>
          val local = ctx.method.lookupLocal(param).get
          ctx1 = genLoad(arg, ctx1, local.kind)

          val store =
            if (param.name == nme.THIS) STORE_THIS(toTypeKind(ctx1.clazz.symbol.tpe))
            else STORE_LOCAL(local)

          store setPos arg.pos
      }

      // store arguments in reverse order on the stack
      ctx1.bb.emit(stores.reverse)
      ctx1
    }

    private def genLoadArguments(args: List[Tree], tpes: List[Type], ctx: Context): Context =
      (args zip tpes).foldLeft(ctx) {
        case (res, (arg, tpe)) =>
          genLoad(arg, res, toTypeKind(tpe))
      }

    private def genLoadModule(ctx: Context, tree: Tree): Context = {
      // Working around SI-5604.  Rather than failing the compile when we see
      // a package here, check if there's a package object.
      val sym = (
        if (!tree.symbol.isPackageClass) tree.symbol
        else tree.symbol.info.member(nme.PACKAGE) match {
          case NoSymbol => abort("Cannot use package as value: " + tree)
          case s        => debugwarn("Bug: found package class where package object expected.  Converting.") ; s.moduleClass
        }
      )
      debuglog("LOAD_MODULE from %s: %s".format(tree.shortClass, sym))
      ctx.bb.emit(LOAD_MODULE(sym), tree.pos)
      ctx
    }

    def genConversion(from: TypeKind, to: TypeKind, ctx: Context, cast: Boolean) = {
      if (cast)
        ctx.bb.emit(CALL_PRIMITIVE(Conversion(from, to)))
      else {
        ctx.bb.emit(DROP(from))
        ctx.bb.emit(CONSTANT(Constant(from == to)))
      }
    }

    def genCast(from: TypeKind, to: TypeKind, ctx: Context, cast: Boolean) =
      ctx.bb.emit(if (cast) CHECK_CAST(to) else IS_INSTANCE(to))

    def getZeroOf(k: TypeKind): Instruction = k match {
      case UNIT            => CONSTANT(Constant(()))
      case BOOL            => CONSTANT(Constant(false))
      case BYTE            => CONSTANT(Constant(0: Byte))
      case SHORT           => CONSTANT(Constant(0: Short))
      case CHAR            => CONSTANT(Constant(0: Char))
      case INT             => CONSTANT(Constant(0: Int))
      case LONG            => CONSTANT(Constant(0: Long))
      case FLOAT           => CONSTANT(Constant(0.0f))
      case DOUBLE          => CONSTANT(Constant(0.0d))
      case REFERENCE(cls)  => CONSTANT(Constant(null: Any))
      case ARRAY(elem)     => CONSTANT(Constant(null: Any))
      case BOXED(_)        => CONSTANT(Constant(null: Any))
      case ConcatClass     => abort("no zero of ConcatClass")
    }


    /** Is the given symbol a primitive operation? */
    def isPrimitive(fun: Symbol): Boolean = scalaPrimitives.isPrimitive(fun)

    /** Generate coercion denoted by "code"
     */
    def genCoercion(tree: Tree, ctx: Context, code: Int) = {
      import scalaPrimitives._
      (code: @switch) match {
        case B2B => ()
        case B2C => ctx.bb.emit(CALL_PRIMITIVE(Conversion(BYTE, CHAR)), tree.pos)
        case B2S => ctx.bb.emit(CALL_PRIMITIVE(Conversion(BYTE, SHORT)), tree.pos)
        case B2I => ctx.bb.emit(CALL_PRIMITIVE(Conversion(BYTE, INT)), tree.pos)
        case B2L => ctx.bb.emit(CALL_PRIMITIVE(Conversion(BYTE, LONG)), tree.pos)
        case B2F => ctx.bb.emit(CALL_PRIMITIVE(Conversion(BYTE, FLOAT)), tree.pos)
        case B2D => ctx.bb.emit(CALL_PRIMITIVE(Conversion(BYTE, DOUBLE)), tree.pos)

        case S2B => ctx.bb.emit(CALL_PRIMITIVE(Conversion(SHORT, BYTE)), tree.pos)
        case S2S => ()
        case S2C => ctx.bb.emit(CALL_PRIMITIVE(Conversion(SHORT, CHAR)), tree.pos)
        case S2I => ctx.bb.emit(CALL_PRIMITIVE(Conversion(SHORT, INT)), tree.pos)
        case S2L => ctx.bb.emit(CALL_PRIMITIVE(Conversion(SHORT, LONG)), tree.pos)
        case S2F => ctx.bb.emit(CALL_PRIMITIVE(Conversion(SHORT, FLOAT)), tree.pos)
        case S2D => ctx.bb.emit(CALL_PRIMITIVE(Conversion(SHORT, DOUBLE)), tree.pos)

        case C2B => ctx.bb.emit(CALL_PRIMITIVE(Conversion(CHAR, BYTE)), tree.pos)
        case C2S => ctx.bb.emit(CALL_PRIMITIVE(Conversion(CHAR, SHORT)), tree.pos)
        case C2C => ()
        case C2I => ctx.bb.emit(CALL_PRIMITIVE(Conversion(CHAR, INT)), tree.pos)
        case C2L => ctx.bb.emit(CALL_PRIMITIVE(Conversion(CHAR, LONG)), tree.pos)
        case C2F => ctx.bb.emit(CALL_PRIMITIVE(Conversion(CHAR, FLOAT)), tree.pos)
        case C2D => ctx.bb.emit(CALL_PRIMITIVE(Conversion(CHAR, DOUBLE)), tree.pos)

        case I2B => ctx.bb.emit(CALL_PRIMITIVE(Conversion(INT, BYTE)), tree.pos)
        case I2S => ctx.bb.emit(CALL_PRIMITIVE(Conversion(INT, SHORT)), tree.pos)
        case I2C => ctx.bb.emit(CALL_PRIMITIVE(Conversion(INT, CHAR)), tree.pos)
        case I2I => ()
        case I2L => ctx.bb.emit(CALL_PRIMITIVE(Conversion(INT, LONG)), tree.pos)
        case I2F => ctx.bb.emit(CALL_PRIMITIVE(Conversion(INT, FLOAT)), tree.pos)
        case I2D => ctx.bb.emit(CALL_PRIMITIVE(Conversion(INT, DOUBLE)), tree.pos)

        case L2B => ctx.bb.emit(CALL_PRIMITIVE(Conversion(LONG, BYTE)), tree.pos)
        case L2S => ctx.bb.emit(CALL_PRIMITIVE(Conversion(LONG, SHORT)), tree.pos)
        case L2C => ctx.bb.emit(CALL_PRIMITIVE(Conversion(LONG, CHAR)), tree.pos)
        case L2I => ctx.bb.emit(CALL_PRIMITIVE(Conversion(LONG, INT)), tree.pos)
        case L2L => ()
        case L2F => ctx.bb.emit(CALL_PRIMITIVE(Conversion(LONG, FLOAT)), tree.pos)
        case L2D => ctx.bb.emit(CALL_PRIMITIVE(Conversion(LONG, DOUBLE)), tree.pos)

        case F2B => ctx.bb.emit(CALL_PRIMITIVE(Conversion(FLOAT, BYTE)), tree.pos)
        case F2S => ctx.bb.emit(CALL_PRIMITIVE(Conversion(FLOAT, SHORT)), tree.pos)
        case F2C => ctx.bb.emit(CALL_PRIMITIVE(Conversion(FLOAT, CHAR)), tree.pos)
        case F2I => ctx.bb.emit(CALL_PRIMITIVE(Conversion(FLOAT, INT)), tree.pos)
        case F2L => ctx.bb.emit(CALL_PRIMITIVE(Conversion(FLOAT, LONG)), tree.pos)
        case F2F => ()
        case F2D => ctx.bb.emit(CALL_PRIMITIVE(Conversion(FLOAT, DOUBLE)), tree.pos)

        case D2B => ctx.bb.emit(CALL_PRIMITIVE(Conversion(DOUBLE, BYTE)), tree.pos)
        case D2S => ctx.bb.emit(CALL_PRIMITIVE(Conversion(DOUBLE, SHORT)), tree.pos)
        case D2C => ctx.bb.emit(CALL_PRIMITIVE(Conversion(DOUBLE, CHAR)), tree.pos)
        case D2I => ctx.bb.emit(CALL_PRIMITIVE(Conversion(DOUBLE, INT)), tree.pos)
        case D2L => ctx.bb.emit(CALL_PRIMITIVE(Conversion(DOUBLE, LONG)), tree.pos)
        case D2F => ctx.bb.emit(CALL_PRIMITIVE(Conversion(DOUBLE, FLOAT)), tree.pos)
        case D2D => ()

        case _ => abort("Unknown coercion primitive: " + code)
      }
    }

    /** The Object => String overload.
     */
    private lazy val String_valueOf: Symbol = getMember(StringModule, nme.valueOf) filter (sym =>
      sym.info.paramTypes match {
        case List(pt) => pt.typeSymbol == ObjectClass
        case _        => false
      }
    )

    // I wrote it this way before I realized all the primitive types are
    // boxed at this point, so I'd have to unbox them.  Keeping it around in
    // case we want to get more precise.
    //
    // private def valueOfForType(tp: Type): Symbol = {
    //   val xs = getMember(StringModule, nme.valueOf) filter (sym =>
    //     // We always exclude the Array[Char] overload because java throws an NPE if
    //     // you pass it a null.  It will instead find the Object one, which doesn't.
    //     sym.info.paramTypes match {
    //       case List(pt) => pt.typeSymbol != ArrayClass && (tp <:< pt)
    //       case _        => false
    //     }
    //   )
    //   xs.alternatives match {
    //     case List(sym)  => sym
    //     case _          => NoSymbol
    //   }
    // }

    /** Generate string concatenation.
     *
     *  @param tree ...
     *  @param ctx  ...
     *  @return     ...
     */
    def genStringConcat(tree: Tree, ctx: Context): Context = {
      liftStringConcat(tree) match {
        // Optimization for expressions of the form "" + x.  We can avoid the StringBuilder.
        case List(Literal(Constant("")), arg) if !forMSIL =>
          debuglog("Rewriting \"\" + x as String.valueOf(x) for: " + arg)
          val ctx1 = genLoad(arg, ctx, ObjectReference)
          ctx1.bb.emit(CALL_METHOD(String_valueOf, Static(false)), arg.pos)
          ctx1
        case concatenations =>
          debuglog("Lifted string concatenations for " + tree + "\n to: " + concatenations)
          var ctx1 = ctx
          ctx1.bb.emit(CALL_PRIMITIVE(StartConcat), tree.pos)
          for (elem <- concatenations) {
            val kind = toTypeKind(elem.tpe)
            ctx1 = genLoad(elem, ctx1, kind)
            ctx1.bb.emit(CALL_PRIMITIVE(StringConcat(kind)), elem.pos)
          }
          ctx1.bb.emit(CALL_PRIMITIVE(EndConcat), tree.pos)
          ctx1
      }
    }

    /** Generate the scala ## method.
     */
    def genScalaHash(tree: Tree, ctx: Context): Context = {
      val hashMethod = {
        ctx.bb.emit(LOAD_MODULE(ScalaRunTimeModule))
        getMember(ScalaRunTimeModule, nme.hash_)
      }

      val ctx1 = genLoad(tree, ctx, ObjectReference)
      ctx1.bb.emit(CALL_METHOD(hashMethod, Static(false)))
      ctx1
    }

    /**
     * Returns a list of trees that each should be concatenated, from
     * left to right. It turns a chained call like "a".+("b").+("c") into
     * a list of arguments.
     */
    def liftStringConcat(tree: Tree): List[Tree] = tree match {
      case Apply(fun @ Select(larg, method), rarg) =>
        if (isPrimitive(fun.symbol) &&
            scalaPrimitives.getPrimitive(fun.symbol) == scalaPrimitives.CONCAT)
          liftStringConcat(larg) ::: rarg
        else
          List(tree)
      case _ =>
        List(tree)
    }

    /** Some useful equality helpers.
     */
    def isNull(t: Tree) = cond(t) { case Literal(Constant(null)) => true }

    /* If l or r is constant null, returns the other ; otherwise null */
    def ifOneIsNull(l: Tree, r: Tree) = if (isNull(l)) r else if (isNull(r)) l else null

    /**
     * Find the label denoted by `lsym` and enter it in context `ctx`.
     *
     * We only enter one symbol at a time, even though we might traverse the same
     * tree more than once per method. That's because we cannot enter labels that
     * might be duplicated (for instance, inside finally blocks).
     *
     * TODO: restrict the scanning to smaller subtrees than the whole method.
     *  It is sufficient to scan the trees of the innermost enclosing block.
     */
    private def resolveForwardLabel(tree: Tree, ctx: Context, lsym: Symbol): Unit = tree foreachPartial {
      case t @ LabelDef(_, params, rhs) if t.symbol == lsym =>
        ctx.labels.getOrElseUpdate(t.symbol, {
          val locals  = params map (p => new Local(p.symbol, toTypeKind(p.symbol.info), false))
          ctx.method addLocals locals

          new Label(t.symbol) setParams (params map (_.symbol))
        })
        rhs
    }

    /**
     * Generate code for conditional expressions. The two basic blocks
     * represent the continuation in case of success/failure of the
     * test.
     */
    private def genCond(tree: Tree,
                        ctx: Context,
                        thenCtx: Context,
                        elseCtx: Context): Unit =
    {
      def genComparisonOp(l: Tree, r: Tree, code: Int) {
        val op: TestOp = code match {
          case scalaPrimitives.LT => LT
          case scalaPrimitives.LE => LE
          case scalaPrimitives.GT => GT
          case scalaPrimitives.GE => GE
          case scalaPrimitives.ID | scalaPrimitives.EQ => EQ
          case scalaPrimitives.NI | scalaPrimitives.NE => NE

          case _ => abort("Unknown comparison primitive: " + code)
        }

        // special-case reference (in)equality test for null (null eq x, x eq null)
        lazy val nonNullSide = ifOneIsNull(l, r)
        if (isReferenceEqualityOp(code) && nonNullSide != null) {
          val ctx1 = genLoad(nonNullSide, ctx, ObjectReference)
          ctx1.bb.emitOnly(
            CZJUMP(thenCtx.bb, elseCtx.bb, op, ObjectReference)
          )
        }
        else {
          val kind = getMaxType(l.tpe :: r.tpe :: Nil)
          var ctx1 = genLoad(l, ctx, kind)
          ctx1 = genLoad(r, ctx1, kind)

          ctx1.bb.emitOnly(
            CJUMP(thenCtx.bb, elseCtx.bb, op, kind) setPos r.pos
          )
        }
      }

      debuglog("Entering genCond with tree: " + tree);

      // the default emission
      def default() = {
        val ctx1 = genLoad(tree, ctx, BOOL)
        ctx1.bb.closeWith(CZJUMP(thenCtx.bb, elseCtx.bb, NE, BOOL) setPos tree.pos)
      }

      tree match {
        // The comparison symbol is in ScalaPrimitives's "primitives" map
        case Apply(fun, args) if isPrimitive(fun.symbol) =>
          import scalaPrimitives.{ ZNOT, ZAND, ZOR, EQ, getPrimitive }

          // lhs and rhs of test
          lazy val Select(lhs, _) = fun
          lazy val rhs = args.head

          def genZandOrZor(and: Boolean) = {
            val ctxInterm = ctx.newBlock

            if (and) genCond(lhs, ctx, ctxInterm, elseCtx)
            else genCond(lhs, ctx, thenCtx, ctxInterm)

            genCond(rhs, ctxInterm, thenCtx, elseCtx)
          }
          def genRefEq(isEq: Boolean) = {
            val f = genEqEqPrimitive(lhs, rhs, ctx) _
            if (isEq) f(thenCtx, elseCtx)
            else f(elseCtx, thenCtx)
          }

          getPrimitive(fun.symbol) match {
            case ZNOT   => genCond(lhs, ctx, elseCtx, thenCtx)
            case ZAND   => genZandOrZor(and = true)
            case ZOR    => genZandOrZor(and = false)
            case code   =>
              // x == y where LHS is reference type
              if (isUniversalEqualityOp(code) && toTypeKind(lhs.tpe).isReferenceType) {
                if (code == EQ) genRefEq(isEq = true)
                else genRefEq(isEq = false)
              }
              else if (isComparisonOp(code))
                genComparisonOp(lhs, rhs, code)
              else
                default
          }

        case _ => default
      }
    }

    /**
     * Generate the "==" code for object references. It is equivalent of
     * if (l eq null) r eq null else l.equals(r);
     *
     * @param l       left-hand side of the '=='
     * @param r       right-hand side of the '=='
     * @param ctx     current context
     * @param thenCtx target context if the comparison yields true
     * @param elseCtx target context if the comparison yields false
     */
    def genEqEqPrimitive(l: Tree, r: Tree, ctx: Context)(thenCtx: Context, elseCtx: Context): Unit = {
      def getTempLocal = ctx.method.lookupLocal(nme.EQEQ_LOCAL_VAR) getOrElse {
        ctx.makeLocal(l.pos, AnyRefClass.tpe, nme.EQEQ_LOCAL_VAR)
      }

      /** True if the equality comparison is between values that require the use of the rich equality
        * comparator (scala.runtime.Comparator.equals). This is the case when either side of the
        * comparison might have a run-time type subtype of java.lang.Number or java.lang.Character.
        * When it is statically known that both sides are equal and subtypes of Number of Character,
        * not using the rich equality is possible (their own equals method will do ok.)*/
      def mustUseAnyComparator: Boolean = {
        def areSameFinals = l.tpe.isFinalType && r.tpe.isFinalType && (l.tpe =:= r.tpe)
        !areSameFinals && isMaybeBoxed(l.tpe.typeSymbol) && isMaybeBoxed(r.tpe.typeSymbol)
      }

      if (mustUseAnyComparator) {
        // when -optimise is on we call the @inline-version of equals, found in ScalaRunTime
        val equalsMethod =
          if (!settings.optimise.value) {
            def default = platform.externalEquals
            platform match {
              case x: JavaPlatform =>
                import x._
                  if (l.tpe <:< BoxedNumberClass.tpe) {
                    if (r.tpe <:< BoxedNumberClass.tpe) externalEqualsNumNum
                    else if (r.tpe <:< BoxedCharacterClass.tpe) externalEqualsNumChar
                    else externalEqualsNumObject
                  }
                  else default

              case _ => default
            }
          }
          else {
            ctx.bb.emit(LOAD_MODULE(ScalaRunTimeModule))
            getMember(ScalaRunTimeModule, nme.inlinedEquals)
          }

        val ctx1 = genLoad(l, ctx, ObjectReference)
        val ctx2 = genLoad(r, ctx1, ObjectReference)
        ctx2.bb.emitOnly(
          CALL_METHOD(equalsMethod, if (settings.optimise.value) Dynamic else Static(false)),
          CZJUMP(thenCtx.bb, elseCtx.bb, NE, BOOL)
        )
      }
      else {
        if (isNull(l))
          // null == expr -> expr eq null
          genLoad(r, ctx, ObjectReference).bb emitOnly CZJUMP(thenCtx.bb, elseCtx.bb, EQ, ObjectReference)
        else if (isNull(r)) {
          // expr == null -> expr eq null
          genLoad(l, ctx, ObjectReference).bb emitOnly CZJUMP(thenCtx.bb, elseCtx.bb, EQ, ObjectReference)
        } else {
          val eqEqTempLocal = getTempLocal
          var ctx1 = genLoad(l, ctx, ObjectReference)
          lazy val nonNullCtx = ctx1.newBlock

          // l == r -> if (l eq null) r eq null else l.equals(r)
          ctx1 = genLoad(r, ctx1, ObjectReference)
          val nullCtx = ctx1.newBlock

          ctx1.bb.emitOnly(
            STORE_LOCAL(eqEqTempLocal) setPos l.pos,
            DUP(ObjectReference),
            CZJUMP(nullCtx.bb, nonNullCtx.bb, EQ, ObjectReference)
          )
          nullCtx.bb.emitOnly(
            DROP(ObjectReference) setPos l.pos, // type of AnyRef
            LOAD_LOCAL(eqEqTempLocal),
            CZJUMP(thenCtx.bb, elseCtx.bb, EQ, ObjectReference)
          )
          nonNullCtx.bb.emitOnly(
            LOAD_LOCAL(eqEqTempLocal) setPos l.pos,
            CALL_METHOD(Object_equals, Dynamic),
            CZJUMP(thenCtx.bb, elseCtx.bb, NE, BOOL)
          )
        }
      }
    }

    /**
     * Add all fields of the given class symbol to the current ICode
     * class.
     */
    private def addClassFields(ctx: Context, cls: Symbol) {
      debugassert(ctx.clazz.symbol eq cls,
               "Classes are not the same: " + ctx.clazz.symbol + ", " + cls)

      /** Non-method term members are fields, except for module members. Module
       *  members can only happen on .NET (no flatten) for inner traits. There,
       *  a module symbol is generated (transformInfo in mixin) which is used
       *  as owner for the members of the implementation class (so that the
       *  backend emits them as static).
       *  No code is needed for this module symbol.
       */
      for (f <- cls.info.decls ; if !f.isMethod && f.isTerm && !f.isModule)
        ctx.clazz addField new IField(f)
    }

    /**
     * Add parameters to the current ICode method. It is assumed the methods
     * have been uncurried, so the list of lists contains just one list.
     */
    private def addMethodParams(ctx: Context, vparamss: List[List[ValDef]]) {
      vparamss match {
        case Nil => ()

        case vparams :: Nil =>
          for (p <- vparams) {
            val lv = new Local(p.symbol, toTypeKind(p.symbol.info), true)
            ctx.method.addParam(lv)
            ctx.scope.add(lv)
            ctx.bb.varsInScope += lv
          }
          ctx.method.params = ctx.method.params.reverse

        case _ =>
          abort("Malformed parameter list: " + vparamss)
      }
    }

    /** Does this tree have a try-catch block? */
    def mayCleanStack(tree: Tree): Boolean = tree exists {
      case Try(_, _, _) => true
      case _            => false
    }

    /**
     *  If the block consists of a single unconditional jump, prune
     *  it by replacing the instructions in the predecessor to jump
     *  directly to the JUMP target of the block.
     *
     *  @param method ...
     */
    def prune(method: IMethod) = {
      var changed = false
      var n = 0

      def prune0(block: BasicBlock): Unit = {
        val optCont = block.lastInstruction match {
          case JUMP(b) if (b != block) => Some(b)
          case _ => None
        }
        if (block.size == 1 && optCont.isDefined) {
          val Some(cont) = optCont;
          val pred = block.predecessors;
          debuglog("Preds: " + pred + " of " + block + " (" + optCont + ")");
          pred foreach { p =>
            changed = true
            p.lastInstruction match {
              case CJUMP(succ, fail, cond, kind) if (succ == block || fail == block) =>
                debuglog("Pruning empty if branch.");
                p.replaceInstruction(p.lastInstruction,
                                     if (block == succ)
                                       if (block == fail)
                                         CJUMP(cont, cont, cond, kind)
                                       else
                                         CJUMP(cont, fail, cond, kind)
                                     else if (block == fail)
                                       CJUMP(succ, cont, cond, kind)
                                     else
                                       abort("Could not find block in preds: " + method + " " + block + " " + pred + " " + p))

              case CZJUMP(succ, fail, cond, kind) if (succ == block || fail == block) =>
                debuglog("Pruning empty ifz branch.");
                p.replaceInstruction(p.lastInstruction,
                                     if (block == succ)
                                       if (block == fail)
                                         CZJUMP(cont, cont, cond, kind)
                                       else
                                         CZJUMP(cont, fail, cond, kind)
                                     else if (block == fail)
                                       CZJUMP(succ, cont, cond, kind)
                                     else
                                       abort("Could not find block in preds"))

              case JUMP(b) if (b == block) =>
                debuglog("Pruning empty JMP branch.");
                val replaced = p.replaceInstruction(p.lastInstruction, JUMP(cont))
                debugassert(replaced, "Didn't find p.lastInstruction")

              case SWITCH(tags, labels) if (labels contains block) =>
                debuglog("Pruning empty SWITCH branch.");
                p.replaceInstruction(p.lastInstruction,
                                     SWITCH(tags, labels map (l => if (l == block) cont else l)))

              // the last instr of the predecessor `p` is not a jump to the block `block`.
              // this happens when `block` is part of an exception handler covering `b`.
              case _ => ()
            }
          }
          if (changed) {
            debuglog("Removing block: " + block)
            method.code.removeBlock(block)
            for (e <- method.exh) {
              e.covered = e.covered filter (_ != block)
              e.blocks  = e.blocks filter (_ != block)
              if (e.startBlock eq block)
                e setStartBlock cont;
            }
          }
        }
      }

      do {
        changed = false
        n += 1
        method.blocks foreach prune0
      } while (changed)

      debuglog("Prune fixpoint reached in " + n + " iterations.");
    }

    def getMaxType(ts: List[Type]): TypeKind =
      ts map toTypeKind reduceLeft (_ maxType _)

    /** Tree transformer that duplicates code and at the same time creates
     *  fresh symbols for existing labels. Since labels may be used before
     *  they are defined (forward jumps), all labels found are mapped to fresh
     *  symbols. References to the same label (use or definition) will remain
     *  consistent after this transformation (both the use and the definition of
     *  some label l will be mapped to the same label l').
     *
     *  Note: If the tree fragment passed to the duplicator contains unbound
     *  label names, the bind to the outer labeldef will be lost! That's because
     *  a use of an unbound label l will be transformed to l', and the corresponding
     *  label def, being outside the scope of this transformation, will not be updated.
     *
     *  All LabelDefs are entered into the context label map, since it makes no sense
     *  to delay it any more: they will be used at some point.
     */
    class DuplicateLabels(boundLabels: Set[Symbol]) extends Transformer {
      val labels = perRunCaches.newMap[Symbol, Symbol]()
      var method: Symbol = _
      var ctx: Context = _

      def apply(ctx: Context, t: Tree) = {
        this.method = ctx.method.symbol
        this.ctx = ctx
        transform(t)
      }

      override def transform(t: Tree): Tree = {
        val sym = t.symbol
        def getLabel(pos: Position, name: Name) =
          labels.getOrElseUpdate(sym,
            method.newLabel(unit.freshTermName(name.toString), sym.pos) setInfo sym.tpe
          )

        t match {
          case t @ Apply(_, args) if sym.isLabel && !boundLabels(sym) =>
            val newSym = getLabel(sym.pos, sym.name)
            val tree = Apply(global.gen.mkAttributedRef(newSym), transformTrees(args)) setPos t.pos
            tree.tpe = t.tpe
            tree

          case t @ LabelDef(name, params, rhs) =>
            val newSym = getLabel(t.pos, name)
            val tree = treeCopy.LabelDef(t, newSym.name, params, transform(rhs))
            tree.symbol = newSym

            val pair = (newSym -> (new Label(newSym) setParams (params map (_.symbol))))
            log("Added " + pair + " to labels.")
            ctx.labels += pair
            ctx.method.addLocals(params map (p => new Local(p.symbol, toTypeKind(p.symbol.info), false)))

            tree

          case _ => super.transform(t)
        }
      }
    }

    /////////////////////// Context ////////////////////////////////

    abstract class Cleanup(val value: AnyRef) {
      def contains(x: AnyRef) = value == x
    }
    case class MonitorRelease(m: Local) extends Cleanup(m) { }
    case class Finalizer(f: Tree, ctx: Context) extends Cleanup (f) { }

    def duplicateFinalizer(boundLabels: Set[Symbol], targetCtx: Context, finalizer: Tree) =  {
      (new DuplicateLabels(boundLabels))(targetCtx, finalizer)
    }

    def savingCleanups[T](ctx: Context)(body: => T): T = {
      val saved = ctx.cleanups
      try body
      finally ctx.cleanups = saved
    }

    /**
     * The Context class keeps information relative to the current state
     * in code generation
     */
    class Context {
      /** The current package. */
      var packg: Name = _

      /** The current class. */
      var clazz: IClass = _

      /** The current method. */
      var method: IMethod = _

      /** The current basic block. */
      var bb: BasicBlock = _

      /** Map from label symbols to label objects. */
      var labels = perRunCaches.newMap[Symbol, Label]()

      /** Current method definition. */
      var defdef: DefDef = _

      /** current exception handlers */
      var handlers: List[ExceptionHandler] = Nil

      /** The current monitors or finalizers, to be cleaned up upon `return`. */
      var cleanups: List[Cleanup] = Nil

      /** The exception handlers we are currently generating code for */
      var currentExceptionHandlers: List[ExceptionHandler] = Nil

      /** The current local variable scope. */
      var scope: Scope = EmptyScope

      var handlerCount = 0

      override def toString(): String = {
        val buf = new StringBuilder()
        buf.append("\tpackage: ").append(packg).append('\n')
        buf.append("\tclazz: ").append(clazz).append('\n')
        buf.append("\tmethod: ").append(method).append('\n')
        buf.append("\tbb: ").append(bb).append('\n')
        buf.append("\tlabels: ").append(labels).append('\n')
        buf.append("\texception handlers: ").append(handlers).append('\n')
        buf.append("\tcleanups: ").append(cleanups).append('\n')
        buf.append("\tscope: ").append(scope).append('\n')
        buf.toString()
      }

      def loadException(ctx: Context, exh: ExceptionHandler, pos: Position) = {
        debuglog("Emitting LOAD_EXCEPTION for class: " + exh.loadExceptionClass)
        ctx.bb.emit(LOAD_EXCEPTION(exh.loadExceptionClass) setPos pos, pos)
      }

      def this(other: Context) = {
        this()
        this.packg = other.packg
        this.clazz = other.clazz
        this.method = other.method
        this.bb = other.bb
        this.labels = other.labels
        this.defdef = other.defdef
        this.handlers = other.handlers
        this.handlerCount = other.handlerCount
        this.cleanups = other.cleanups
        this.currentExceptionHandlers = other.currentExceptionHandlers
        this.scope = other.scope
      }

      def setPackage(p: Name): this.type = {
        this.packg = p
        this
      }

      def setClass(c: IClass): this.type = {
        this.clazz = c
        this
      }

      def setMethod(m: IMethod): this.type = {
        this.method = m
        this
      }

      def setBasicBlock(b: BasicBlock): this.type = {
        this.bb = b
        this
      }

      def enterSynchronized(monitor: Local): this.type = {
        cleanups = MonitorRelease(monitor) :: cleanups
        this
      }

      def exitSynchronized(monitor: Local): this.type = {
        assert(cleanups.head contains monitor,
               "Bad nesting of cleanup operations: " + cleanups + " trying to exit from monitor: " + monitor)
        cleanups = cleanups.tail
        this
      }

      def addFinalizer(f: Tree, ctx: Context): this.type = {
        cleanups = Finalizer(f, ctx) :: cleanups;
        this
      }

      def removeFinalizer(f: Tree): this.type = {
        assert(cleanups.head contains f,
               "Illegal nesting of cleanup operations: " + cleanups + " while exiting finalizer " + f);
        cleanups = cleanups.tail
        this
      }

      /** Prepare a new context upon entry into a method.
       *
       *  @param m ...
       *  @param d ...
       *  @return  ...
       */
      def enterMethod(m: IMethod, d: DefDef): Context = {
        val ctx1 = new Context(this) setMethod(m)
        ctx1.labels = mutable.HashMap()
        ctx1.method.code = new Code(m)
        ctx1.bb = ctx1.method.startBlock
        ctx1.defdef = d
        ctx1.scope = EmptyScope
        ctx1.enterScope
        ctx1
      }

      /** Return a new context for a new basic block. */
      def newBlock(): Context = {
        val block = method.code.newBlock
        handlers foreach (_ addCoveredBlock block)
        currentExceptionHandlers foreach (_ addBlock block)
        block.varsInScope.clear()
        block.varsInScope ++= scope.varsInScope
        new Context(this) setBasicBlock block
      }

      def enterScope() {
        scope = new Scope(scope)
      }

      def exitScope() {
        if (bb.nonEmpty) {
          scope.locals foreach { lv => bb.emit(SCOPE_EXIT(lv)) }
        }
        scope = scope.outer
      }

      /** Create a new exception handler and adds it in the list
       * of current exception handlers. All new blocks will be
       * 'covered' by this exception handler (in addition to the
       * previously active handlers).
       */
      private def newExceptionHandler(cls: Symbol, resultKind: TypeKind, pos: Position): ExceptionHandler = {
        handlerCount += 1
        val exh = new ExceptionHandler(method, newTermNameCached("" + handlerCount), cls, pos)
        exh.resultKind = resultKind
        method.addHandler(exh)
        handlers = exh :: handlers
        debuglog("added handler: " + exh);

        exh
      }

      /** Add an active exception handler in this context. It will cover all new basic blocks
       *  created from now on. */
      private def addActiveHandler(exh: ExceptionHandler) {
        handlerCount += 1
        handlers = exh :: handlers
        debuglog("added handler: " + exh);
      }

      /** Return a new context for generating code for the given
       * exception handler.
       */
      private def enterExceptionHandler(exh: ExceptionHandler): Context = {
        currentExceptionHandlers ::= exh
        val ctx = newBlock
        exh.setStartBlock(ctx.bb)
        ctx
      }

      def endHandler() {
        currentExceptionHandlers = currentExceptionHandlers.tail
      }

      /** Remove the given handler from the list of active exception handlers. */
      def removeActiveHandler(exh: ExceptionHandler): Unit = {
        assert(handlerCount > 0 && handlers.head == exh,
               "Wrong nesting of exception handlers." + this + " for " + exh)
        handlerCount -= 1
        handlers = handlers.tail
        debuglog("removed handler: " + exh);

      }

      /** Clone the current context */
      def dup: Context = new Context(this)

      /** Make a fresh local variable. It ensures the 'name' is unique. */
      def makeLocal(pos: Position, tpe: Type, name: String): Local = {
        val sym = method.symbol.newVariable(unit.freshTermName(name), pos, Flags.SYNTHETIC) setInfo tpe
        this.method.addLocal(new Local(sym, toTypeKind(tpe), false))
      }


      /**
       * Generate exception handlers for the body. Body is evaluated
       * with a context where all the handlers are active. Handlers are
       * evaluated in the 'outer' context.
       *
       * It returns the resulting context, with the same active handlers as
       * before the call. Use it like:
       *
       * <code> ctx.Try( ctx => {
       *   ctx.bb.emit(...) // protected block
       * }, (ThrowableClass,
       *   ctx => {
       *     ctx.bb.emit(...); // exception handler
       *   }), (AnotherExceptionClass,
       *   ctx => {...
       *   } ))</code>
       */
      def Try(body: Context => Context,
              handlers: List[(Symbol, TypeKind, Context => Context)],
              finalizer: Tree,
              tree: Tree) = if (forMSIL) TryMsil(body, handlers, finalizer, tree) else {

        val outerCtx = this.dup       // context for generating exception handlers, covered by finalizer
        val finalizerCtx = this.dup   // context for generating finalizer handler
        val afterCtx = outerCtx.newBlock
        var tmp: Local = null
        val kind = toTypeKind(tree.tpe)
        val guardResult = kind != UNIT && mayCleanStack(finalizer)
        // we need to save bound labels before any code generation is performed on
        // the current context (otherwise, any new labels in the finalizer that need to
        // be duplicated would be incorrectly considered bound -- see #2850).
        val boundLabels: Set[Symbol] = Set.empty ++ labels.keySet

        if (guardResult) {
          tmp = this.makeLocal(tree.pos, tree.tpe, "tmp")
        }

        def emitFinalizer(ctx: Context): Context = if (!finalizer.isEmpty) {
          val ctx1 = finalizerCtx.dup.newBlock
          ctx.bb.closeWith(JUMP(ctx1.bb))

          if (guardResult) {
            ctx1.bb.emit(STORE_LOCAL(tmp))
            val ctx2 = genLoad(duplicateFinalizer(boundLabels, ctx1, finalizer), ctx1, UNIT)
            ctx2.bb.emit(LOAD_LOCAL(tmp))
            ctx2
          } else
            genLoad(duplicateFinalizer(boundLabels, ctx1, finalizer), ctx1, UNIT)
        } else ctx


        val finalizerExh = if (finalizer != EmptyTree) Some({
          val exh = outerCtx.newExceptionHandler(NoSymbol, toTypeKind(finalizer.tpe), finalizer.pos) // finalizer covers exception handlers
          this.addActiveHandler(exh)  // .. and body aswell
          val ctx = finalizerCtx.enterExceptionHandler(exh)
          val exception = ctx.makeLocal(finalizer.pos, ThrowableClass.tpe, "exc")
          loadException(ctx, exh, finalizer.pos)
          ctx.bb.emit(STORE_LOCAL(exception));
          val ctx1 = genLoad(finalizer, ctx, UNIT);
          ctx1.bb.emit(LOAD_LOCAL(exception));
          ctx1.bb.emit(THROW(ThrowableClass));
          ctx1.bb.enterIgnoreMode;
          ctx1.bb.close
          finalizerCtx.endHandler()
          exh
        }) else None

        val exhs = handlers.map { case (sym, kind, handler) =>  // def genWildcardHandler(sym: Symbol): (Symbol, TypeKind, Context => Context) =
            val exh = this.newExceptionHandler(sym, kind, tree.pos)
            var ctx1 = outerCtx.enterExceptionHandler(exh)
            ctx1.addFinalizer(finalizer, finalizerCtx)
            loadException(ctx1, exh, tree.pos)
            ctx1 = handler(ctx1)
            // emit finalizer
            val ctx2 = emitFinalizer(ctx1)
            ctx2.bb.closeWith(JUMP(afterCtx.bb))
            outerCtx.endHandler()
            exh
          }
        val bodyCtx = this.newBlock
        if (finalizer != EmptyTree)
          bodyCtx.addFinalizer(finalizer, finalizerCtx)

        var finalCtx = body(bodyCtx)
        finalCtx = emitFinalizer(finalCtx)

        outerCtx.bb.closeWith(JUMP(bodyCtx.bb))

        finalCtx.bb.closeWith(JUMP(afterCtx.bb))

        afterCtx
      }


      /** try-catch-finally blocks are actually simpler to emit in MSIL, because there
       *  is support for `finally` in bytecode.
       *
       *  A
       *    try { .. } catch { .. } finally { .. }
       *  block is de-sugared into
       *    try { try { ..} catch { .. } } finally { .. }
       *
       *  In ICode `finally` block is represented exactly the same as an exception handler,
       *  but with `NoSymbol` as the exception class. The covered blocks are all blocks of
       *  the `try { .. } catch { .. }`.
       *
       *  Also, TryMsil does not enter any Finalizers into the `cleanups`, because the
       *  CLI takes care of running the finalizer when seeing a `leave` statement inside
       *  a try / catch.
       */
      def TryMsil(body: Context => Context,
                  handlers: List[(Symbol, TypeKind, (Context => Context))],
                  finalizer: Tree,
                  tree: Tree) = {

        val outerCtx = this.dup       // context for generating exception handlers, covered by finalizer
        val finalizerCtx = this.dup   // context for generating finalizer handler
        val afterCtx = outerCtx.newBlock

        if (finalizer != EmptyTree) {
          // finalizer is covers try and all catch blocks, i.e.
          //   try { try { .. } catch { ..} } finally { .. }
          val exh = outerCtx.newExceptionHandler(NoSymbol, UNIT, tree.pos)
          this.addActiveHandler(exh)
          val ctx = finalizerCtx.enterExceptionHandler(exh)
          loadException(ctx, exh, tree.pos)
          val ctx1 = genLoad(finalizer, ctx, UNIT)
          // need jump for the ICode to be valid. MSIL backend will emit `Endfinally` instead.
          ctx1.bb.closeWith(JUMP(afterCtx.bb))
          finalizerCtx.endHandler()
        }

        for (handler <- handlers) {
          val exh = this.newExceptionHandler(handler._1, handler._2, tree.pos)
          var ctx1 = outerCtx.enterExceptionHandler(exh)
          loadException(ctx1, exh, tree.pos)
          ctx1 = handler._3(ctx1)
          // msil backend will emit `Leave` to jump out of a handler
          ctx1.bb.closeWith(JUMP(afterCtx.bb))
          outerCtx.endHandler()
        }

        val bodyCtx = this.newBlock

        val finalCtx = body(bodyCtx)

        outerCtx.bb.closeWith(JUMP(bodyCtx.bb))

        // msil backend will emit `Leave` to jump out of a try-block
        finalCtx.bb.closeWith(JUMP(afterCtx.bb))

        afterCtx
      }
    }
  }

    /**
     * Represent a label in the current method code. In order
     * to support forward jumps, labels can be created without
     * having a deisgnated target block. They can later be attached
     * by calling `anchor`.
     */
    class Label(val symbol: Symbol) {
      var anchored = false
      var block: BasicBlock = _
      var params: List[Symbol] = _

      private var toPatch: List[Instruction] = Nil

      /** Fix this label to the given basic block. */
      def anchor(b: BasicBlock): Label = {
        assert(!anchored, "Cannot anchor an already anchored label!")
        anchored = true
        this.block = b
        this
      }

      def setParams(p: List[Symbol]): Label = {
        assert(params eq null, "Cannot set label parameters twice!")
        params = p
        this
      }

      /** Add an instruction that refers to this label. */
      def addCallingInstruction(i: Instruction) =
        toPatch = i :: toPatch;

      /**
       * Patch the code by replacing pseudo call instructions with
       * jumps to the given basic block.
       */
      def patch(code: Code) {
        val map = mapFrom(toPatch)(patch)
        code.blocks foreach (_ subst map)
      }

      /**
       * Return the patched instruction. If the given instruction
       * jumps to this label, replace it with the basic block. Otherwise,
       * return the same instruction. Conditional jumps have more than one
       * label, so they are replaced only if all labels are anchored.
       */
      def patch(instr: Instruction): Instruction = {
        assert(anchored, "Cannot patch until this label is anchored: " + this)

        instr match {
          case PJUMP(self)
          if (self == this) => JUMP(block)

          case PCJUMP(self, failure, cond, kind)
          if (self == this && failure.anchored) =>
            CJUMP(block, failure.block, cond, kind)

          case PCJUMP(success, self, cond, kind)
          if (self == this && success.anchored) =>
            CJUMP(success.block, block, cond, kind)

          case PCZJUMP(self, failure, cond, kind)
          if (self == this && failure.anchored) =>
            CZJUMP(block, failure.block, cond, kind)

          case PCZJUMP(success, self, cond, kind)
          if (self == this && success.anchored) =>
            CZJUMP(success.block, block, cond, kind)

          case _ => instr
        }
      }

      override def toString() = symbol.toString()
    }

    ///////////////// Fake instructions //////////////////////////

    /**
     * Pseudo jump: it takes a Label instead of a basic block.
     * It is used temporarily during code generation. It is replaced
     * by a real JUMP instruction when all labels are resolved.
     */
    abstract class PseudoJUMP(label: Label) extends Instruction {
      override def toString(): String = "PJUMP " + label.symbol

      override def consumed = 0
      override def produced = 0

      // register with the given label
      if (!label.anchored)
        label.addCallingInstruction(this);
    }

    case class PJUMP(whereto: Label) extends PseudoJUMP(whereto)

    case class PCJUMP(success: Label, failure: Label, cond: TestOp, kind: TypeKind)
    extends PseudoJUMP(success) {
      override def toString(): String =
        "PCJUMP (" + kind + ") " + success.symbol.simpleName +
        " : " + failure.symbol.simpleName

      if (!failure.anchored)
        failure.addCallingInstruction(this)
    }

    case class PCZJUMP(success: Label, failure: Label, cond: TestOp, kind: TypeKind)
    extends PseudoJUMP(success) {
      override def toString(): String =
        "PCZJUMP (" + kind + ") " + success.symbol.simpleName +
        " : " + failure.symbol.simpleName

      if (!failure.anchored)
        failure.addCallingInstruction(this)
    }

  /** Local variable scopes. Keep track of line numbers for debugging info. */
  class Scope(val outer: Scope) {
    val locals: ListBuffer[Local] = new ListBuffer

    def add(l: Local)     = locals += l
    def remove(l: Local)  = locals -= l

    /** Return all locals that are in scope. */
    def varsInScope: Buffer[Local] = outer.varsInScope.clone() ++= locals

    override def toString() = locals.mkString(outer.toString + "[", ", ", "]")
  }

  object EmptyScope extends Scope(null) {
    override def toString() = "[]"
    override def varsInScope: Buffer[Local] = new ListBuffer
  }
}
