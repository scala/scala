/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package backend
package icode

import scala.collection.mutable.{Map, HashMap, ListBuffer, Buffer, HashSet}
import scala.tools.nsc.symtab._
import scala.tools.nsc.util.Position


/** This class ...
 *
 *  @author  Iulian Dragos
 *  @version 1.0
 */
// TODO:
// - switches with alternatives
abstract class GenICode extends SubComponent  {
  import global._
  import icodes._
  import icodes.opcodes._

  val phaseName = "icode"

  override def newPhase(prev: Phase) = new ICodePhase(prev)

  class ICodePhase(prev: Phase) extends StdPhase(prev) {

    override def description = "Generate ICode from the AST"

    var unit: CompilationUnit = _

    // We assume definitions are alread initialized
    val STRING = REFERENCE(definitions.StringClass)

    // this depends on the backend! should be changed.
    val ANY_REF_CLASS = REFERENCE(definitions.ObjectClass)

    val SCALA_ALL    = REFERENCE(definitions.NothingClass)
    val SCALA_ALLREF = REFERENCE(definitions.NullClass)
    val THROWABLE    = REFERENCE(definitions.ThrowableClass)

    val BoxesRunTime_equals =
      if (!forMSIL)
        definitions.getMember(definitions.BoxesRunTimeClass, nme.equals_)
      else
        definitions.getMember(definitions.getClass("scala.runtime.Comparator").linkedModuleOfClass, nme.equals_)

    override def run {
      scalaPrimitives.init
      classes.clear
      super.run
    }

    override def apply(unit: CompilationUnit): Unit = {
      this.unit = unit
      unit.icode.clear
      log("Generating icode for " + unit)
      gen(unit.body)
      this.unit = null
    }

    def gen(tree: Tree): Context = gen(tree, new Context())

    def gen(trees: List[Tree], ctx: Context): Context = {
      var ctx1 = ctx
      for (t <- trees) ctx1 = gen(t, ctx1)
      ctx1
    }

    /////////////////// Code generation ///////////////////////

    def gen(tree: Tree, ctx: Context): Context = tree match {
      case EmptyTree => ctx

      case PackageDef(pid, stats) =>
        gen(stats, ctx setPackage pid.name)

      case ClassDef(mods, name, _, impl) =>
        log("Generating class: " + tree.symbol.fullNameString)
        val outerClass = ctx.clazz
        ctx setClass (new IClass(tree.symbol) setCompilationUnit unit)
        addClassFields(ctx, tree.symbol);
        classes += (tree.symbol -> ctx.clazz)
        unit.icode += ctx.clazz
        gen(impl, ctx)
        ctx setClass outerClass

      // !! modules should be eliminated by refcheck... or not?
      case ModuleDef(mods, name, impl) =>
        abort("Modules should not reach backend!")

      case ValDef(mods, name, tpt, rhs) =>
        ctx // we use the symbol to add fields

      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        if (settings.debug.value)
          log("Entering method " + name)
        val m = new IMethod(tree.symbol)
        m.sourceFile = unit.source.toString()
        m.returnType = if (tree.symbol.isConstructor) UNIT
                       else toTypeKind(tree.symbol.info.resultType)
        ctx.clazz.addMethod(m)

        var ctx1 = ctx.enterMethod(m, tree.asInstanceOf[DefDef])
        addMethodParams(ctx1, vparamss)
        m.native = m.symbol.hasAnnotation(definitions.NativeAttr)

        if (!m.isDeferred && !m.native) {
          ctx1 = genLoad(rhs, ctx1, m.returnType);

          // reverse the order of the local variables, to match the source-order
          m.locals = m.locals.reverse

          rhs match {
            case Block(_, Return(_)) => ()
            case Return(_) => ()
            case EmptyTree =>
              error("Concrete method has no definition: " + tree)
            case _ => if (ctx1.bb.isEmpty)
              ctx1.bb.emit(RETURN(m.returnType), rhs.pos)
            else
              ctx1.bb.emit(RETURN(m.returnType))
          }
          ctx1.bb.close
          prune(ctx1.method)
        } else
          ctx1.method.setCode(null)
        ctx1

      case Template(_, _, body) =>
        gen(body, ctx)

      case _ =>
        abort("Illegal tree in gen: " + tree)
    }

    private def genStat(trees: List[Tree], ctx: Context): Context = {
      var currentCtx = ctx

      for (t <- trees)
        currentCtx = genStat(t, currentCtx)

      currentCtx
    }

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
    private def genStat(tree: Tree, ctx: Context): Context = {

      tree match {
        case Assign(lhs @ Select(_, _), rhs) =>
          if (lhs.symbol.isStaticMember) {
            val ctx1 = genLoad(rhs, ctx, toTypeKind(lhs.symbol.info))
            ctx1.bb.emit(STORE_FIELD(lhs.symbol, true), tree.pos)
            ctx1
          } else {
            var ctx1 = genLoadQualifier(lhs, ctx)
            ctx1 = genLoad(rhs, ctx1, toTypeKind(lhs.symbol.info))
            ctx1.bb.emit(STORE_FIELD(lhs.symbol, false), tree.pos)
            ctx1
          }

        case Assign(lhs, rhs) =>
          val ctx1 = genLoad(rhs, ctx, toTypeKind(lhs.symbol.info))
          val Some(l) = ctx.method.lookupLocal(lhs.symbol)
          ctx1.bb.emit(STORE_LOCAL(l), tree.pos)
          ctx1

        case _ =>
          genLoad(tree, ctx, UNIT)
      }
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
      if (settings.debug.value)
        log("at line: " + (if (tree.pos.isDefined) tree.pos.line else tree.pos))

      /**
       * Generate code for primitive arithmetic operations.
       */
      def genArithmeticOp(tree: Tree, ctx: Context, code: Int): Context = {
        val Apply(fun @ Select(larg, _), args) = tree
        var ctx1 = ctx
        var resKind = toTypeKind(larg.tpe)

        if (settings.debug.value) {
          assert(args.length <= 1,
                 "Too many arguments for primitive function: " + fun.symbol)
          assert(resKind.isNumericType | resKind == BOOL,
                 resKind.toString() + " is not a numeric or boolean type " +
                 "[operation: " + fun.symbol + "]")
        }

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
                abort("Unknown unary operation: " + fun.symbol.fullNameString +
                      " code: " + code)
            }
            generatedType = resKind

          // binary operation
          case rarg :: Nil =>
            resKind = getMaxType(larg.tpe :: rarg.tpe :: Nil);
            if (scalaPrimitives.isShiftOp(code) || scalaPrimitives.isBitwiseOp(code))
              assert(resKind.isIntType | resKind == BOOL,
                   resKind.toString() + " incompatible with arithmetic modulo operation: " + ctx1);

            ctx1 = genLoad(larg, ctx1, resKind);
            ctx1 = genLoad(rarg,
                           ctx1,  // check .NET size of shift arguments!
                           if (scalaPrimitives.isShiftOp(code)) INT else resKind)

            generatedType = resKind
            code match {
              case scalaPrimitives.ADD =>
                ctx1.bb.emit(CALL_PRIMITIVE(Arithmetic(ADD, resKind)), tree.pos)
              case scalaPrimitives.SUB =>
                ctx1.bb.emit(CALL_PRIMITIVE(Arithmetic(SUB, resKind)), tree.pos)
              case scalaPrimitives.MUL =>
                ctx1.bb.emit(CALL_PRIMITIVE(Arithmetic(MUL, resKind)), tree.pos)
              case scalaPrimitives.DIV =>
                ctx1.bb.emit(CALL_PRIMITIVE(Arithmetic(DIV, resKind)), tree.pos)
              case scalaPrimitives.MOD =>
                ctx1.bb.emit(CALL_PRIMITIVE(Arithmetic(REM, resKind)), tree.pos)
              case scalaPrimitives.OR  =>
                ctx1.bb.emit(CALL_PRIMITIVE(Logical(OR, resKind)), tree.pos)
              case scalaPrimitives.XOR =>
                ctx1.bb.emit(CALL_PRIMITIVE(Logical(XOR, resKind)), tree.pos)
              case scalaPrimitives.AND =>
                ctx1.bb.emit(CALL_PRIMITIVE(Logical(AND, resKind)), tree.pos)
              case scalaPrimitives.LSL =>
                ctx1.bb.emit(CALL_PRIMITIVE(Shift(LSL, resKind)), tree.pos)
                generatedType = resKind
              case scalaPrimitives.LSR =>
                ctx1.bb.emit(CALL_PRIMITIVE(Shift(LSR, resKind)), tree.pos)
                generatedType = resKind
              case scalaPrimitives.ASR =>
                ctx1.bb.emit(CALL_PRIMITIVE(Shift(ASR, resKind)), tree.pos)
                generatedType = resKind
              case _ =>
                abort("Unknown primitive: " + fun.symbol + "[" + code + "]")
            }

          case _ =>
            abort("Too many arguments for primitive function: " + tree)
        }
        ctx1
      }

      /** Generate primitive array operations.
       *
       *  @param tree ...
       *  @param ctx  ...
       *  @param code ...
       *  @return     ...
       */
      def genArrayOp(tree: Tree, ctx: Context, code: Int): Context = {
        import scalaPrimitives._
        val Apply(Select(arrayObj, _), args) = tree
        val k = toTypeKind(arrayObj.tpe)
        val ARRAY(elem) = k
        var ctx1 = genLoad(arrayObj, ctx, k)

        if (scalaPrimitives.isArrayGet(code)) {
          // load argument on stack
          if (settings.debug.value)
            assert(args.length == 1,
                   "Too many arguments for array get operation: " + tree);
          ctx1 = genLoad(args.head, ctx1, INT)
          generatedType = elem
        } else if (scalaPrimitives.isArraySet(code)) {
          if (settings.debug.value)
            assert(args.length == 2,
                   "Too many arguments for array set operation: " + tree);
          ctx1 = genLoad(args.head, ctx1, INT)
          ctx1 = genLoad(args.tail.head, ctx1, toTypeKind(args.tail.head.tpe))
          // the following line should really be here, but because of bugs in erasure
          // we pretend we generate whatever type is expected from us.
          //generatedType = UNIT
        } else
          generatedType = INT

        code match {
          case ZARRAY_LENGTH =>
            ctx1.bb.emit(CALL_PRIMITIVE(ArrayLength(BOOL)), tree.pos)
          case BARRAY_LENGTH =>
            ctx1.bb.emit(CALL_PRIMITIVE(ArrayLength(BYTE)), tree.pos)
          case SARRAY_LENGTH =>
            ctx1.bb.emit(CALL_PRIMITIVE(ArrayLength(SHORT)), tree.pos)
          case CARRAY_LENGTH =>
            ctx1.bb.emit(CALL_PRIMITIVE(ArrayLength(CHAR)), tree.pos)
          case IARRAY_LENGTH =>
            ctx1.bb.emit(CALL_PRIMITIVE(ArrayLength(INT)), tree.pos)
          case LARRAY_LENGTH =>
            ctx1.bb.emit(CALL_PRIMITIVE(ArrayLength(LONG)), tree.pos)
          case FARRAY_LENGTH =>
            ctx1.bb.emit(CALL_PRIMITIVE(ArrayLength(FLOAT)), tree.pos)
          case DARRAY_LENGTH =>
            ctx1.bb.emit(CALL_PRIMITIVE(ArrayLength(DOUBLE)), tree.pos)
          case OARRAY_LENGTH =>
            ctx1.bb.emit(CALL_PRIMITIVE(ArrayLength(ANY_REF_CLASS)), tree.pos)

          case ZARRAY_GET =>
            ctx1.bb.emit(LOAD_ARRAY_ITEM(BOOL), tree.pos)
          case BARRAY_GET =>
            ctx1.bb.emit(LOAD_ARRAY_ITEM(BYTE), tree.pos)
          case SARRAY_GET =>
            ctx1.bb.emit(LOAD_ARRAY_ITEM(SHORT), tree.pos)
          case CARRAY_GET =>
            ctx1.bb.emit(LOAD_ARRAY_ITEM(CHAR), tree.pos)
          case IARRAY_GET =>
            ctx1.bb.emit(LOAD_ARRAY_ITEM(INT), tree.pos)
          case LARRAY_GET =>
            ctx1.bb.emit(LOAD_ARRAY_ITEM(LONG), tree.pos)
          case FARRAY_GET =>
            ctx1.bb.emit(LOAD_ARRAY_ITEM(FLOAT), tree.pos)
          case DARRAY_GET =>
            ctx1.bb.emit(LOAD_ARRAY_ITEM(DOUBLE), tree.pos)
          case OARRAY_GET =>
            ctx1.bb.emit(LOAD_ARRAY_ITEM(ANY_REF_CLASS), tree.pos)

          case ZARRAY_SET =>
            ctx1.bb.emit(STORE_ARRAY_ITEM(BOOL), tree.pos)
          case BARRAY_SET =>
            ctx1.bb.emit(STORE_ARRAY_ITEM(BYTE), tree.pos)
          case SARRAY_SET =>
            ctx1.bb.emit(STORE_ARRAY_ITEM(SHORT), tree.pos)
          case CARRAY_SET =>
            ctx1.bb.emit(STORE_ARRAY_ITEM(CHAR), tree.pos)
          case IARRAY_SET =>
            ctx1.bb.emit(STORE_ARRAY_ITEM(INT), tree.pos)
          case LARRAY_SET =>
            ctx1.bb.emit(STORE_ARRAY_ITEM(LONG), tree.pos)
          case FARRAY_SET =>
            ctx1.bb.emit(STORE_ARRAY_ITEM(FLOAT), tree.pos)
          case DARRAY_SET =>
            ctx1.bb.emit(STORE_ARRAY_ITEM(DOUBLE), tree.pos)
          case OARRAY_SET =>
            ctx1.bb.emit(STORE_ARRAY_ITEM(ANY_REF_CLASS), tree.pos)

          case _ =>
            abort("Unknown operation on arrays: " + tree + " code: " + code)
        }
        ctx1
      }

      // genLoad
      val resCtx: Context = tree match {
        case LabelDef(name, params, rhs) =>
          val ctx1 = ctx.newBlock
          if (isLoopHeaderLabel(name))
            ctx1.bb.loopHeader = true;

          ctx1.labels.get(tree.symbol) match {
            case Some(label) =>
              label.anchor(ctx1.bb)
              label.patch(ctx.method.code)

            case None =>
              ctx1.labels += (tree.symbol -> (new Label(tree.symbol) anchor ctx1.bb setParams (params map (_.symbol))));
              ctx.method.addLocals(params map (p => new Local(p.symbol, toTypeKind(p.symbol.info), false)));
              if (settings.debug.value)
                log("Adding label " + tree.symbol);
          }

          ctx.bb.emit(JUMP(ctx1.bb), tree.pos)
          ctx.bb.close
          genLoad(rhs, ctx1, expectedType /*toTypeKind(tree.symbol.info.resultType)*/)

        case ValDef(_, nme.THIS, _, _) =>
          if (settings.debug.value) log("skipping trivial assign to _$this: " + tree)
          ctx

        case ValDef(_, _, _, rhs) =>
          val sym = tree.symbol
          val local = ctx.method.addLocal(new Local(sym, toTypeKind(sym.info), false))

          if (rhs == EmptyTree) {
            if (settings.debug.value)
              log("Uninitialized variable " + tree + " at: " + (tree.pos));
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

        case If(cond, thenp, elsep) =>
          var thenCtx = ctx.newBlock
          var elseCtx = ctx.newBlock
          val contCtx = ctx.newBlock
            genCond(cond, ctx, thenCtx, elseCtx)
          val ifKind = toTypeKind(tree.tpe)

          val thenKind = toTypeKind(thenp.tpe)
          val elseKind = if (elsep == EmptyTree) UNIT else toTypeKind(elsep.tpe)

          generatedType = ifKind

          // we need to drop unneeded results, if one branch gives
          // unit and the other gives something on the stack, because
          // the type of 'if' is scala.Any, and its erasure would be Object.
          // But unboxed units are not Objects...
          if (thenKind == UNIT || elseKind == UNIT) {
            if (settings.debug.value)
              log("Will drop result from an if branch");
            thenCtx = genLoad(thenp, thenCtx, UNIT)
            elseCtx = genLoad(elsep, elseCtx, UNIT)
            if (settings.debug.value)
              assert(expectedType == UNIT,
                     "I produce UNIT in a context where " +
                     expectedType + " is expected!")
            generatedType = UNIT
          } else {
            thenCtx = genLoad(thenp, thenCtx, ifKind)
            elseCtx = genLoad(elsep, elseCtx, ifKind)
          }

          thenCtx.bb.emit(JUMP(contCtx.bb))
          thenCtx.bb.close
          if (elsep == EmptyTree)
            elseCtx.bb.emit(JUMP(contCtx.bb), tree.pos)
          else
            elseCtx.bb.emit(JUMP(contCtx.bb))
          elseCtx.bb.close

          contCtx

        case Return(expr) =>
          val returnedKind = toTypeKind(expr.tpe)
          var ctx1 = genLoad(expr, ctx, returnedKind)
          val oldcleanups = ctx1.cleanups
          lazy val tmp = ctx1.makeLocal(tree.pos, expr.tpe, "tmp")
          var saved = false

          for (op <- ctx1.cleanups) op match {
            case MonitorRelease(m) =>
              if (settings.debug.value) log("removing " + m + " from cleanups: " + ctx1.cleanups)
              ctx1.bb.emit(LOAD_LOCAL(m))
              ctx1.bb.emit(MONITOR_EXIT())
              ctx1.exitSynchronized(m)
            case Finalizer(f) =>
              if (settings.debug.value) log("removing " + f + " from cleanups: " + ctx1.cleanups)
              if (returnedKind != UNIT && mayCleanStack(f) && !saved) {
                ctx1.bb.emit(STORE_LOCAL(tmp))
                saved = true
              }
              // we have to run this without the same finalizer in
              // the list, otherwise infinite recursion happens for
              // finalizers that contain 'return'
              ctx1 = genLoad(f, ctx1.removeFinalizer(f), UNIT)
          }
          ctx1.cleanups = oldcleanups

          if (saved) ctx1.bb.emit(LOAD_LOCAL(tmp))
          adapt(returnedKind, ctx1.method.returnType, ctx1, tree.pos)
          ctx1.bb.emit(RETURN(ctx.method.returnType), tree.pos)
          ctx1.bb.enterIgnoreMode
          generatedType = expectedType
          ctx1

        case Try(block, catches, finalizer) =>
          val kind = toTypeKind(tree.tpe)
          var tmp: Local = null
          val guardResult = kind != UNIT && mayCleanStack(finalizer)
          if (guardResult) {
            tmp = ctx.makeLocal(tree.pos, tree.tpe, "tmp")
          }
          def duplicateFinalizer =
            (new DuplicateLabels(ctx.labels.keySet))(ctx, finalizer)

          var handlers = for (CaseDef(pat, _, body) <- catches.reverse)
            yield pat match {
              case Typed(Ident(nme.WILDCARD), tpt) => (tpt.tpe.typeSymbol, kind, {
                ctx: Context =>
                  ctx.bb.emit(DROP(REFERENCE(tpt.tpe.typeSymbol)));
                  val ctx1 = genLoad(body, ctx, kind);
                  if (guardResult) {
                    ctx1.bb.emit(STORE_LOCAL(tmp))
                    val ctx2 = genLoad(duplicateFinalizer, ctx1, UNIT)
                    ctx2.bb.emit(LOAD_LOCAL(tmp))
                    ctx2
                  } else
                    genLoad(duplicateFinalizer, ctx1, UNIT);
                })

              case Ident(nme.WILDCARD) => (definitions.ThrowableClass, kind, {
                ctx: Context =>
                  ctx.bb.emit(DROP(REFERENCE(definitions.ThrowableClass)))
                  val ctx1 = genLoad(body, ctx, kind)
                  if (guardResult) {
                    ctx1.bb.emit(STORE_LOCAL(tmp))
                    val ctx2 = genLoad(duplicateFinalizer, ctx1, UNIT)
                    ctx2.bb.emit(LOAD_LOCAL(tmp))
                    ctx2
                  } else
                    genLoad(duplicateFinalizer, ctx1, UNIT)
                })

              case Bind(name, _) =>
                val exception = ctx.method.addLocal(new Local(pat.symbol, toTypeKind(pat.symbol.tpe), false))

                (pat.symbol.tpe.typeSymbol, kind, {
                  ctx: Context =>
                    ctx.bb.emit(STORE_LOCAL(exception), pat.pos);
                    val ctx1 = genLoad(body, ctx, kind);
                    if (guardResult) {
                      ctx1.bb.emit(STORE_LOCAL(tmp))
                      val ctx2 = genLoad(duplicateFinalizer, ctx1, UNIT)
                      ctx2.bb.emit(LOAD_LOCAL(tmp))
                      ctx2
                    } else
                      genLoad(duplicateFinalizer, ctx1, UNIT);
                })
            }

          ctx.Try(
            bodyCtx => {
              generatedType = kind; //toTypeKind(block.tpe);
              val ctx1 = genLoad(block, bodyCtx, generatedType);
              if (guardResult) {
                val tmp = ctx1.makeLocal(tree.pos, tree.tpe, "tmp")
                ctx1.bb.emit(STORE_LOCAL(tmp))
                val ctx2 = genLoad(duplicateFinalizer, ctx1, UNIT)
                ctx2.bb.emit(LOAD_LOCAL(tmp))
                ctx2
              } else
                genLoad(duplicateFinalizer, ctx1, UNIT)
            },
            handlers,
            finalizer)

        case Throw(expr) =>
          val ctx1 = genLoad(expr, ctx, THROWABLE)
          ctx1.bb.emit(THROW(), tree.pos)
          ctx1.bb.enterIgnoreMode
          generatedType = SCALA_ALL
          ctx1

        case New(tpt) =>
          abort("Unexpected New")

        case Apply(TypeApply(fun, targs), _) =>
          val sym = fun.symbol
          var ctx1 = ctx
          var cast = false

          if (sym == definitions.Object_isInstanceOf)
            cast = false
          else if (sym == definitions.Object_asInstanceOf)
            cast = true
          else
            abort("Unexpected type application " + fun + "[sym: " + sym.fullNameString + "]" + " in: " + tree)

          val Select(obj, _) = fun
          val l = toTypeKind(obj.tpe)
          val r = toTypeKind(targs.head.tpe)

          ctx1 = genLoadQualifier(fun, ctx)

          if (l.isValueType && r.isValueType)
            genConversion(l, r, ctx1, cast)
          else if (l.isValueType) {
            ctx1.bb.emit(DROP(l), fun.pos)
            if (cast) {
              ctx1.bb.emit(NEW(REFERENCE(definitions.getClass("ClassCastException"))))
              ctx1.bb.emit(DUP(ANY_REF_CLASS))
              ctx1.bb.emit(THROW())
            } else
              ctx1.bb.emit(CONSTANT(Constant(false)))
          }
          else if (r.isValueType && cast) {
            assert(false, tree) /* Erasure should have added an unboxing operation to prevent that. */
          }
          else if (r.isValueType)
            ctx.bb.emit(IS_INSTANCE(REFERENCE(definitions.boxedClass(r.toType.typeSymbol))))
          else
            genCast(l, r, ctx1, cast);

          generatedType = if (cast) r else BOOL;
          ctx1

        // 'super' call: Note: since constructors are supposed to
        // return an instance of what they construct, we have to take
        // special care. On JVM they are 'void', and Scala forbids (syntactically)
        // to call super constructors explicitly and/or use their 'returned' value.
        // therefore, we can ignore this fact, and generate code that leaves nothing
        // on the stack (contrary to what the type in the AST says).
        case Apply(fun @ Select(Super(_, mix), _), args) =>
          if (settings.debug.value)
            log("Call to super: " + tree);
          val invokeStyle = SuperCall(mix)
//            if (fun.symbol.isConstructor) Static(true) else SuperCall(mix);

          ctx.bb.emit(THIS(ctx.clazz.symbol), tree.pos)
          val ctx1 = genLoadArguments(args, fun.symbol.info.paramTypes, ctx)

          ctx1.bb.emit(CALL_METHOD(fun.symbol, invokeStyle), tree.pos)
          generatedType =
            if (fun.symbol.isConstructor) UNIT
            else toTypeKind(fun.symbol.info.resultType)
          ctx1

        // 'new' constructor call: Note: since constructors are
        // thought to return an instance of what they construct,
        // we have to 'simulate' it by DUPlicating the freshly created
        // instance (on JVM, <init> methods return VOID).
        case Apply(fun @ Select(New(tpt), nme.CONSTRUCTOR), args) =>
          val ctor = fun.symbol
          if (settings.debug.value)
            assert(ctor.isClassConstructor,
                   "'new' call to non-constructor: " + ctor.name)

          generatedType = toTypeKind(tpt.tpe)
          if (settings.debug.value)
            assert(generatedType.isReferenceType || generatedType.isArrayType,
                 "Non reference type cannot be instantiated: " + generatedType)

          var ctx1 = ctx

          generatedType match {
            case arr @ ARRAY(elem) =>
              ctx1 = genLoadArguments(args, ctor.info.paramTypes, ctx)
              val dims = arr.dimensions
              var elemKind = arr.elementKind
              if (args.length > dims)
                unit.error(tree.pos, "too many arguments for array constructor: found " + args.length +
                  " but array has only " + dims + " dimension(s)")
              if (args.length != dims)
                for (i <- args.length until dims) elemKind = ARRAY(elemKind)
              ctx1.bb.emit(CREATE_ARRAY(elemKind, args.length), tree.pos)

            case rt @ REFERENCE(cls) =>
              if (settings.debug.value)
                assert(ctor.owner == cls,
                       "Symbol " + ctor.owner.fullNameString + " is different than " + tpt)
              val nw = NEW(rt)
              ctx1.bb.emit(nw, tree.pos)
              ctx1.bb.emit(DUP(generatedType))
              ctx1 = genLoadArguments(args, ctor.info.paramTypes, ctx)

              val init = CALL_METHOD(ctor, Static(true))
              nw.init = init
              ctx1.bb.emit(init, tree.pos)

            case _ =>
              abort("Cannot instantiate " + tpt + "of kind: " + generatedType)
          }
          ctx1

        case Apply(fun @ _, List(expr)) if (definitions.isBox(fun.symbol)) =>
          if (settings.debug.value)
            log("BOX : " + fun.symbol.fullNameString);
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

        case Apply(fun @ _, List(expr)) if (definitions.isUnbox(fun.symbol)) =>
          if (settings.debug.value)
            log("UNBOX : " + fun.symbol.fullNameString)
          val ctx1 = genLoad(expr, ctx, toTypeKind(expr.tpe))
          val boxType = toTypeKind(fun.symbol.owner.linkedClassOfClass.tpe)
          generatedType = boxType
          ctx1.bb.emit(UNBOX(boxType), expr.pos)
          ctx1

        case Apply(fun, args) =>
          val sym = fun.symbol

          if (sym.isLabel) {  // jump to a label
            val label = ctx.labels.get(sym) match {
              case Some(l) => l

              // it is a forward jump, scan for labels
              case None =>
                log("Performing scan for label because of forward jump.")
                scanForLabels(ctx.defdef, ctx)
                ctx.labels.get(sym) match {
                  case Some(l) =>
                    log("Found label: " + l)
                    l
                  case _       =>
                    abort("Unknown label target: " + sym +
                          " at: " + (fun.pos) + ": ctx: " + ctx)
                }
            }
            val ctx1 = genLoadLabelArguments(args, label, ctx)
            if (label.anchored)
              ctx1.bb.emit(JUMP(label.block), tree.pos)
            else
              ctx1.bb.emit(PJUMP(label), tree.pos)

            ctx1.bb.close
            ctx1.newBlock
          } else if (isPrimitive(sym)) { // primitive method call
            val Select(receiver, _) = fun

            val code = scalaPrimitives.getPrimitive(sym, receiver.tpe)
            var ctx1 = ctx

            if (scalaPrimitives.isArithmeticOp(code)) {
              ctx1 = genArithmeticOp(tree, ctx1, code)
            } else if (code == scalaPrimitives.CONCAT) {
              ctx1 = genStringConcat(tree, ctx1)
              generatedType = STRING
            } else if (scalaPrimitives.isArrayOp(code)) {
              ctx1 = genArrayOp(tree, ctx1, code)
            } else if (scalaPrimitives.isLogicalOp(code) ||
                       scalaPrimitives.isComparisonOp(code)) {

              val trueCtx = ctx1.newBlock
              val falseCtx = ctx1.newBlock
              val afterCtx = ctx1.newBlock
              genCond(tree, ctx1, trueCtx, falseCtx)
              trueCtx.bb.emit(CONSTANT(Constant(true)), tree.pos)
              trueCtx.bb.emit(JUMP(afterCtx.bb))
              trueCtx.bb.close
              falseCtx.bb.emit(CONSTANT(Constant(false)), tree.pos)
              falseCtx.bb.emit(JUMP(afterCtx.bb))
              falseCtx.bb.close
              generatedType = BOOL
              ctx1 = afterCtx
            } else if (code == scalaPrimitives.SYNCHRONIZED) {
              val monitor = ctx.makeLocal(tree.pos, definitions.ObjectClass.tpe, "monitor")
              ctx1 = genLoadQualifier(fun, ctx1)
              ctx1.bb.emit(DUP(ANY_REF_CLASS))
              ctx1.bb.emit(STORE_LOCAL(monitor))
              ctx1.bb.emit(MONITOR_ENTER(), tree.pos)
              ctx1.enterSynchronized(monitor)

              if (settings.debug.value)
                log("synchronized block start");

              ctx1 = ctx1.Try(
                bodyCtx => {
                  val ctx1 = genLoad(args.head, bodyCtx, expectedType /* toTypeKind(tree.tpe.resultType) */)
                  ctx1.bb.emit(LOAD_LOCAL(monitor))
                  ctx1.bb.emit(MONITOR_EXIT(), tree.pos)
                  ctx1
                }, List(
                  // tree.tpe / fun.tpe is object, which is no longer true after this transformation
                  (NoSymbol, expectedType, exhCtx => {
                  exhCtx.bb.emit(LOAD_LOCAL(monitor))
                  exhCtx.bb.emit(MONITOR_EXIT(), tree.pos)
                  exhCtx.bb.emit(THROW())
                  exhCtx.bb.enterIgnoreMode
                  exhCtx
                })), EmptyTree);
              if (settings.debug.value)
                log("synchronized block end with block " + ctx1.bb +
                    " closed=" + ctx1.bb.closed);
              ctx1.exitSynchronized(monitor)
            } else if (scalaPrimitives.isCoercion(code)) {
              ctx1 = genLoad(receiver, ctx1, toTypeKind(receiver.tpe))
              genCoercion(tree, ctx1, code)
              generatedType = scalaPrimitives.generatedKind(code)
            } else
              abort("Primitive operation not handled yet: " + sym.fullNameString + "(" +
                    fun.symbol.simpleName + ") " + " at: " + (tree.pos));
            ctx1
          } else {  // normal method call
            if (settings.debug.value)
              log("Gen CALL_METHOD with sym: " + sym + " isStaticSymbol: " + sym.isStaticMember);
            var invokeStyle =
              if (sym.isStaticMember)
                Static(false)
              else if (sym.hasFlag(Flags.PRIVATE) || sym.isClassConstructor)
                Static(true)
              else
                Dynamic

            var ctx1 =
              if (invokeStyle.hasInstance) genLoadQualifier(fun, ctx)
              else ctx

            ctx1 = genLoadArguments(args, sym.info.paramTypes, ctx1)

            val hostClass = fun match {
              case Select(qualifier, _)
              if (qualifier.tpe.typeSymbol != definitions.ArrayClass) =>
                qualifier.tpe.typeSymbol
              case _ => sym.owner
            }
            if (settings.debug.value && hostClass != sym.owner)
              log("Set more precise host class for " + sym.fullNameString + " host: " + hostClass);
            ctx1.bb.emit(CALL_METHOD(sym, invokeStyle) setHostClass hostClass, tree.pos)
            if (sym == ctx1.method.symbol) {
              ctx1.method.recursive = true
            }
            generatedType =
              if (sym.isClassConstructor) UNIT
              else toTypeKind(sym.info.resultType);
            ctx1
          }

        case ApplyDynamic(qual, args) =>
          ctx.clazz.bootstrapClass = Some("scala.runtime.DynamicDispatch")
          val ctx1 = genLoad(qual, ctx, ANY_REF_CLASS)
          genLoadArguments(args, tree.symbol.info.paramTypes, ctx1)
          ctx1.bb.emit(CALL_METHOD(tree.symbol, InvokeDynamic), tree.pos)
          ctx1

        case This(qual) =>
          assert(tree.symbol == ctx.clazz.symbol || tree.symbol.isModuleClass,
                 "Trying to access the this of another class: " +
                 "tree.symbol = " + tree.symbol + ", ctx.clazz.symbol = " + ctx.clazz.symbol + " compilation unit:"+unit)
          if (tree.symbol.isModuleClass && tree.symbol != ctx.clazz.symbol) {
            if (settings.debug.value)
              log("LOAD_MODULE from 'This': " + tree.symbol);
            assert(!tree.symbol.isPackageClass, "Cannot use package as value: " + tree)
            ctx.bb.emit(LOAD_MODULE(tree.symbol), tree.pos)
            generatedType = REFERENCE(tree.symbol)
          } else {
            ctx.bb.emit(THIS(ctx.clazz.symbol), tree.pos)
            if (tree.symbol == definitions.ArrayClass)
              generatedType = REFERENCE(definitions.ObjectClass)
            else
              generatedType = REFERENCE(ctx.clazz.symbol)
          }
          ctx

        case Select(Ident(nme.EMPTY_PACKAGE_NAME), module) =>
          if (settings.debug.value) {
            assert(tree.symbol.isModule,
                   "Selection of non-module from empty package: " + tree.toString() +
                   " sym: " + tree.symbol +
                   " at: " + (tree.pos))
            log("LOAD_MODULE from Select(<emptypackage>): " + tree.symbol);
          }
          assert(!tree.symbol.isPackageClass, "Cannot use package as value: " + tree)
          ctx.bb.emit(LOAD_MODULE(tree.symbol), tree.pos)
          ctx

        case Select(qualifier, selector) =>
          val sym = tree.symbol
          generatedType = toTypeKind(sym.info)

          if (sym.isModule) {
            if (settings.debug.value)
              log("LOAD_MODULE from Select(qualifier, selector): " + sym);
            assert(!tree.symbol.isPackageClass, "Cannot use package as value: " + tree)
            ctx.bb.emit(LOAD_MODULE(sym), tree.pos);
            ctx
          } else if (sym.isStaticMember) {
            ctx.bb.emit(LOAD_FIELD(sym, true), tree.pos)
            ctx
          } else {
            val ctx1 = genLoadQualifier(tree, ctx)
            ctx1.bb.emit(LOAD_FIELD(sym, false), tree.pos)
            ctx1
          }

        case Ident(name) =>
          if (!tree.symbol.isPackage) {
            if (tree.symbol.isModule) {
              if (settings.debug.value)
                log("LOAD_MODULE from Ident(name): " + tree.symbol);
              assert(!tree.symbol.isPackageClass, "Cannot use package as value: " + tree)
              ctx.bb.emit(LOAD_MODULE(tree.symbol), tree.pos)
              generatedType = toTypeKind(tree.symbol.info)
            } else {
              try {
                val Some(l) = ctx.method.lookupLocal(tree.symbol)
                ctx.bb.emit(LOAD_LOCAL(l), tree.pos)
                generatedType = l.kind
              } catch {
                case ex: MatchError =>
                  throw new Error("symbol " + tree.symbol +
                                  " does not exist in " + ctx.method)
              }
            }
          }
          ctx

        case Literal(value) =>
          if (value.tag != UnitTag) (value.tag, expectedType) match {
            case (IntTag, LONG) =>
              ctx.bb.emit(CONSTANT(Constant(value.longValue)), tree.pos);
              generatedType = LONG
            case (FloatTag, DOUBLE) =>
              ctx.bb.emit(CONSTANT(Constant(value.doubleValue)), tree.pos);
              generatedType = DOUBLE
            case (NullTag, _) =>
              ctx.bb.emit(CONSTANT(value), tree.pos);
              generatedType = SCALA_ALLREF
            case _ =>
              ctx.bb.emit(CONSTANT(value), tree.pos);
              generatedType = toTypeKind(tree.tpe)
          }
          ctx

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

        case ArrayValue(tpt @ TypeTree(), elems) =>
          var ctx1 = ctx
          val elmKind = toTypeKind(tpt.tpe)
          generatedType = ARRAY(elmKind)

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

        case Match(selector, cases) =>
          if (settings.debug.value)
            log("Generating SWITCH statement.");
          var ctx1 = genLoad(selector, ctx, INT)
          val afterCtx = ctx1.newBlock
          var caseCtx: Context  = null
          generatedType = toTypeKind(tree.tpe)

          var targets: List[BasicBlock] = Nil
          var tags: List[Int] = Nil
          var default: BasicBlock = afterCtx.bb

          for (caze <- cases) caze match {
            case CaseDef(Literal(value), EmptyTree, body) =>
              tags = value.intValue :: tags
              val tmpCtx = ctx1.newBlock
              targets = tmpCtx.bb :: targets

              caseCtx = genLoad(body, tmpCtx , generatedType)
              caseCtx.bb.emit(JUMP(afterCtx.bb), caze.pos)
              caseCtx.bb.close

            case CaseDef(Ident(nme.WILDCARD), EmptyTree, body) =>
              val tmpCtx = ctx1.newBlock
              default = tmpCtx.bb

              caseCtx = genLoad(body, tmpCtx , generatedType)
              caseCtx.bb.emit(JUMP(afterCtx.bb), caze.pos)
              caseCtx.bb.close

            case _ =>
              abort("Invalid case statement in switch-like pattern match: " +
                    tree + " at: " + (tree.pos))
          }
          ctx1.bb.emit(SWITCH(tags.reverse map (x => List(x)),
                             (default :: targets).reverse), tree.pos)
          ctx1.bb.close
          afterCtx

        case EmptyTree =>
          if (expectedType != UNIT)
            ctx.bb.emit(getZeroOf(expectedType))
          ctx

        case _ =>
          abort("Unexpected tree in genLoad: " + tree + " at: " +
                (tree.pos))
      }

      // emit conversion
      if (generatedType != expectedType)
        adapt(generatedType, expectedType, resCtx, tree.pos);

      resCtx
    }

    private def adapt(from: TypeKind, to: TypeKind, ctx: Context, pos: Position): Unit = {
      if (!(from <:< to) && !(from == SCALA_ALLREF && to == SCALA_ALL)) {
        to match {
          case UNIT =>
            ctx.bb.emit(DROP(from), pos)
            if (settings.debug.value)
              log("Dropped an " + from);

          case _ =>
          if (settings.debug.value)
            assert(from != UNIT, "Can't convert from UNIT to " + to + " at: " + pos)
            assert(!from.isReferenceType && !to.isReferenceType, "type error: can't convert from " + from + " to " + to +" in unit "+this.unit)
            ctx.bb.emit(CALL_PRIMITIVE(Conversion(from, to)), pos);
        }
      } else if (from == SCALA_ALL) {
        ctx.bb.emit(THROW())
        ctx.bb.enterIgnoreMode
      } else if (from == SCALA_ALLREF) {
        ctx.bb.emit(DROP(from))
        ctx.bb.emit(CONSTANT(Constant(null)))
      } else (from, to) match  {
        case (BYTE, LONG) | (SHORT, LONG) | (CHAR, LONG) | (INT, LONG) => ctx.bb.emit(CALL_PRIMITIVE(Conversion(INT, LONG)))
        case _ => ()
      }
    }

    /** Load the qualifier of `tree' on top of the stack. */
    private def genLoadQualifier(tree: Tree, ctx: Context): Context =
      tree match {
        case Select(qualifier, _) =>
          genLoad(qualifier, ctx, toTypeKind(qualifier.tpe))
        case _ =>
          abort("Unknown qualifier " + tree)
      }

    /**
     * Generate code that loads args into label parameters.
     */
    private def genLoadLabelArguments(args: List[Tree], label: Label, ctx: Context): Context = {
      if (settings.debug.value)
        assert(args.length == label.params.length,
               "Wrong number of arguments in call to label " + label.symbol)
      var ctx1 = ctx
      var arg = args
      var param = label.params
      val stores: ListBuffer[Instruction] = new ListBuffer

      // store arguments in reverse order on the stack
      while (arg != Nil) {
        arg.head match {
          case This(_) if param.head.name == nme.THIS =>
            //println("skipping trivial argument for " + param.head)
            () // skip trivial arguments
          case Ident(_) if arg.head.symbol == param.head =>
            //println("skipping trivial argument for " + param.head)
           () // skip trivial arguments
          case _ =>
            val Some(l) = ctx.method.lookupLocal(param.head)
            ctx1 = genLoad(arg.head, ctx1, l.kind)
            if (param.head.name == nme.THIS)
              STORE_THIS(toTypeKind(ctx1.clazz.symbol.tpe)).setPos(arg.head.pos) +: stores
            else {
              STORE_LOCAL(l).setPos(arg.head.pos) +: stores
            }
        }
        arg = arg.tail
        param = param.tail
      }

      //println("stores: " + stores)
      ctx1.bb.emit(stores)
      ctx1
    }

    private def genLoadArguments(args: List[Tree], tpes: List[Type], ctx: Context): Context = {
      var ctx1 = ctx
      var arg = args
      var tpe = tpes
      while (arg != Nil) {
        ctx1 = genLoad(arg.head, ctx1, toTypeKind(tpe.head))
        arg = arg.tail
        tpe = tpe.tail
      }
      ctx1
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

    def zeroOf(k: TypeKind): Tree = k match {
      case UNIT            => Literal(())
      case BOOL            => Literal(false)
      case BYTE            => Literal(0: Byte)
      case SHORT           => Literal(0: Short)
      case CHAR            => Literal(0: Char)
      case INT             => Literal(0: Int)
      case LONG            => Literal(0: Long)
      case FLOAT           => Literal(0.0f)
      case DOUBLE          => Literal(0.0d)
      case REFERENCE(cls)  => Literal(null: Any)
      case ARRAY(elem)     => Literal(null: Any)
      case BOXED(_)        => Literal(null: Any)
      case ConcatClass     => abort("no zero of ConcatClass")
    }

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
      code match {
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

    /** Generate string concatenation.
     *
     *  @param tree ...
     *  @param ctx  ...
     *  @return     ...
     */
    def genStringConcat(tree: Tree, ctx: Context): Context = {
      val Apply(Select(larg, _), rarg) = tree
      var ctx1 = ctx

      val concatenations = liftStringConcat(tree)
      if (settings.debug.value)
        log("Lifted string concatenations for " + tree + "\n to: " + concatenations);

      ctx1.bb.emit(CALL_PRIMITIVE(StartConcat), tree.pos);
      for (elem <- concatenations) {
        val kind = toTypeKind(elem.tpe)
        ctx1 = genLoad(elem, ctx1, kind)
        ctx1.bb.emit(CALL_PRIMITIVE(StringConcat(kind)), elem.pos)
      }
      ctx1.bb.emit(CALL_PRIMITIVE(EndConcat), tree.pos)

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


    /**
     * Traverse the tree and store label stubs in the context. This is
     * necessary to handle forward jumps, because at a label application
     * with arguments, the symbols of the corresponding LabelDef parameters
     * are not yet known.
     *
     * Since it is expensive to traverse each method twice, this method is called
     * only when forward jumps really happen, and then it re-traverses the whole
     * method, scanning for LabelDefs.
     *
     * TODO: restrict the scanning to smaller subtrees than the whole method.
     *  It is sufficient to scan the trees of the innermost enclosing block.
     */
    private def scanForLabels(tree: Tree, ctx: Context): Unit =
      new Traverser() {
        override def traverse(tree: Tree): Unit = tree match {

          case LabelDef(name, params, rhs) =>
            if (!ctx.labels.contains(tree.symbol)) {
              ctx.labels += (tree.symbol -> (new Label(tree.symbol) setParams(params map (_.symbol))));
              ctx.method.addLocals(params map (p => new Local(p.symbol, toTypeKind(p.symbol.info), false)));
            }
            super.traverse(rhs)

          case _ =>
            super.traverse(tree)
        }
      } traverse(tree);

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
        // special-case reference (in)equality test for null
        if (code == scalaPrimitives.ID || code == scalaPrimitives.NI) {
          val expr: Tree = (l, r) match {
            case (Literal(Constant(null)), expr) => expr
            case (expr, Literal(Constant(null))) => expr
            case _ => null
          }
          if (expr ne null) {
            val ctx1 = genLoad(expr, ctx, ANY_REF_CLASS)
            if (code == scalaPrimitives.ID)
              ctx1.bb.emit(CZJUMP(thenCtx.bb, elseCtx.bb, EQ, ANY_REF_CLASS))
            else
              ctx1.bb.emit(CZJUMP(thenCtx.bb, elseCtx.bb, NE, ANY_REF_CLASS))
            ctx1.bb.close
            return
          }
        }

        val op: TestOp = code match {
          case scalaPrimitives.LT => LT
          case scalaPrimitives.LE => LE
          case scalaPrimitives.GT => GT
          case scalaPrimitives.GE => GE
          case scalaPrimitives.ID | scalaPrimitives.EQ => EQ
          case scalaPrimitives.NI | scalaPrimitives.NE => NE

          case _ => abort("Unknown comparison primitive: " + code)
        }

        val kind = getMaxType(l.tpe :: r.tpe :: Nil)
        var ctx1 = genLoad(l, ctx, kind);
        ctx1 = genLoad(r, ctx1, kind);
        ctx1.bb.emit(CJUMP(thenCtx.bb, elseCtx.bb, op, kind), r.pos)
        ctx1.bb.close
      }

      /** Log equality tests to file if they are playing with typefire */
      def logEqEq(l: Tree, r: Tree, op: String) {
        def mayBeNumericComparison: Boolean = {
          def isPossiblyBoxed(sym: Symbol): Boolean = {
            import definitions._

            // good enough for now
            (sym == ObjectClass) ||
            (sym isNonBottomSubClass BoxedNumberClass) ||
            (sym isNonBottomSubClass BoxedCharacterClass)
          }

          val lsym = l.tpe.typeSymbol
          val rsym = r.tpe.typeSymbol

          (lsym != rsym) && (isPossiblyBoxed(lsym) && isPossiblyBoxed(rsym))
        }

        val tkl = toTypeKind(l.tpe)
        val tkr = toTypeKind(r.tpe)
        lazy val whereAreWe = tree.pos.source + ":" + tree.pos.line
        def logit(preface: String) =
          runtime.BoxesRunTime.log("[%s] %s %s %s (%s)".format(preface, l.tpe, op, r.tpe, whereAreWe))

        if (tkl.isNumericType && tkr.isNumericType && tkl != tkr)
          logit(" KNOWN ")
        else if (mayBeNumericComparison)
          logit("UNKNOWN")
      }

      if (settings.debug.value)
        log("Entering genCond with tree: " + tree);

      tree match {
        case Apply(fun, args)
          if isPrimitive(fun.symbol) =>
            val code = scalaPrimitives.getPrimitive(fun.symbol)

            if (code == scalaPrimitives.ZNOT) {
              val Select(leftArg, _) = fun
              genCond(leftArg, ctx, elseCtx, thenCtx)
            }
            else if ((code == scalaPrimitives.EQ || code == scalaPrimitives.NE)) {
              val Select(leftArg, _) = fun;
              if (settings.logEquality.value)
                logEqEq(leftArg, args.head, (if (code == scalaPrimitives.EQ) "==" else "!="))

              if (toTypeKind(leftArg.tpe).isReferenceType) {
                if (code == scalaPrimitives.EQ)
                  genEqEqPrimitive(leftArg, args.head, ctx, thenCtx, elseCtx)
                else
                  genEqEqPrimitive(leftArg, args.head, ctx, elseCtx, thenCtx)
              }
              else
                genComparisonOp(leftArg, args.head, code);
            }
            else if (scalaPrimitives.isComparisonOp(code)) {
              val Select(leftArg, _) = fun
              genComparisonOp(leftArg, args.head, code)
            }
            else {
              code match {
                case scalaPrimitives.ZAND =>
                  val Select(leftArg, _) = fun

                  val ctxInterm = ctx.newBlock
                  genCond(leftArg, ctx, ctxInterm, elseCtx)
                  genCond(args.head, ctxInterm, thenCtx, elseCtx)

                case scalaPrimitives.ZOR =>
                  val Select(leftArg, _) = fun

                  val ctxInterm = ctx.newBlock
                  genCond(leftArg, ctx, thenCtx, ctxInterm)
                  genCond(args.head, ctxInterm, thenCtx, elseCtx)

                case _ =>
                  var ctx1 = genLoad(tree, ctx, BOOL)
                  ctx1.bb.emit(CZJUMP(thenCtx.bb, elseCtx.bb, NE, BOOL), tree.pos)
                  ctx1.bb.close
              }
            }

        case _ =>
          var ctx1 = genLoad(tree, ctx, BOOL)
          ctx1.bb.emit(CZJUMP(thenCtx.bb, elseCtx.bb, NE, BOOL), tree.pos)
          ctx1.bb.close
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
    def genEqEqPrimitive(l: Tree, r: Tree, ctx: Context,
                         thenCtx: Context, elseCtx: Context): Unit =
    {

      def eqEqTempName: Name = "eqEqTemp$"

      def getTempLocal: Local = ctx.method.lookupLocal(eqEqTempName) match {
        case Some(local) => local
        case None =>
          val local = ctx.makeLocal(l.pos, definitions.AnyRefClass.typeConstructor, eqEqTempName.toString)
          //assert(!l.pos.source.isEmpty, "bad position, unit = "+unit+", tree = "+l+", pos = "+l.pos.source)
          assert(l.pos.source == unit.source)
          assert(r.pos.source == unit.source)
          local.start = (l.pos).line
          local.end   = (r.pos).line
          local
      }

      /** True if the equality comparison is between values that require the use of the rich equality
        * comparator (scala.runtime.Comparator.equals). This is the case when either side of the
        * comparison might have a run-time type subtype of java.lang.Number or java.lang.Character.
        * When it is statically known that both sides are equal and subtypes of Number of Character,
        * not using the rich equality is possible (their own equals method will do ok.)*/
      def mustUseAnyComparator: Boolean = {
        def isBoxed(sym: Symbol): Boolean =
          ((sym isNonBottomSubClass definitions.BoxedNumberClass) ||
            (!forMSIL && (sym isNonBottomSubClass definitions.BoxedCharacterClass)))

        val lsym = l.tpe.typeSymbol
        val rsym = r.tpe.typeSymbol
        (lsym == definitions.ObjectClass) ||
        (rsym == definitions.ObjectClass) ||
        (lsym != rsym) && (isBoxed(lsym) || isBoxed(rsym))
      }

      if (mustUseAnyComparator) {

        val ctx1 = genLoad(l, ctx, ANY_REF_CLASS)
        val ctx2 = genLoad(r, ctx1, ANY_REF_CLASS)
        ctx2.bb.emit(CALL_METHOD(BoxesRunTime_equals, Static(false)))
        ctx2.bb.emit(CZJUMP(thenCtx.bb, elseCtx.bb, NE, BOOL))
        ctx2.bb.close

      }
      else {

        (l, r) match {
          // null == expr -> expr eq null
          case (Literal(Constant(null)), expr) =>
            val ctx1 = genLoad(expr, ctx, ANY_REF_CLASS)
            ctx1.bb.emit(CZJUMP(thenCtx.bb, elseCtx.bb, EQ, ANY_REF_CLASS))
            ctx1.bb.close

          // expr == null -> if(expr eq null) true else expr.equals(null)
          case (expr, Literal(Constant(null))) =>
            val eqEqTempLocal = getTempLocal
            var ctx1 = genLoad(expr, ctx, ANY_REF_CLASS)
            ctx1.bb.emit(DUP(ANY_REF_CLASS))
            ctx1.bb.emit(STORE_LOCAL(eqEqTempLocal), l.pos)
            val nonNullCtx = ctx1.newBlock
            ctx1.bb.emit(CZJUMP(thenCtx.bb, nonNullCtx.bb, EQ, ANY_REF_CLASS))
            ctx1.bb.close

            nonNullCtx.bb.emit(LOAD_LOCAL(eqEqTempLocal), l.pos)
            nonNullCtx.bb.emit(CONSTANT(Constant(null)), r.pos)
            nonNullCtx.bb.emit(CALL_METHOD(definitions.Object_equals, Dynamic))
            nonNullCtx.bb.emit(CZJUMP(thenCtx.bb, elseCtx.bb, NE, BOOL))
            nonNullCtx.bb.close

          // l == r -> if (l eq null) r eq null else l.equals(r)
          case _ =>
            val eqEqTempLocal = getTempLocal
            var ctx1 = genLoad(l, ctx, ANY_REF_CLASS)
            ctx1 = genLoad(r, ctx1, ANY_REF_CLASS)
            val nullCtx = ctx1.newBlock
            val nonNullCtx = ctx1.newBlock
            ctx1.bb.emit(STORE_LOCAL(eqEqTempLocal), l.pos)
            ctx1.bb.emit(DUP(ANY_REF_CLASS))
            ctx1.bb.emit(CZJUMP(nullCtx.bb, nonNullCtx.bb, EQ, ANY_REF_CLASS))
            ctx1.bb.close

            nullCtx.bb.emit(DROP(ANY_REF_CLASS), l.pos) // type of AnyRef
            nullCtx.bb.emit(LOAD_LOCAL(eqEqTempLocal))
            nullCtx.bb.emit(CZJUMP(thenCtx.bb, elseCtx.bb, EQ, ANY_REF_CLASS))
            nullCtx.bb.close

            nonNullCtx.bb.emit(LOAD_LOCAL(eqEqTempLocal), l.pos)
            nonNullCtx.bb.emit(CALL_METHOD(definitions.Object_equals, Dynamic))
            nonNullCtx.bb.emit(CZJUMP(thenCtx.bb, elseCtx.bb, NE, BOOL))
            nonNullCtx.bb.close
        }
      }
    }

    /**
     * Add all fields of the given class symbol to the current ICode
     * class.
     */
    private def addClassFields(ctx: Context, cls: Symbol) {
      if (settings.debug.value)
        assert(ctx.clazz.symbol eq cls,
               "Classes are not the same: " + ctx.clazz.symbol + ", " + cls)

      for (f <- cls.info.decls.iterator)
        if (!f.isMethod && f.isTerm)
          ctx.clazz.addField(new IField(f));
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
    def mayCleanStack(tree: Tree): Boolean = {
      var hasTry = false
      new Traverser() {
        override def traverse(t: Tree) = t match {
          case Try(_, _, _) => hasTry = true
          case _ => super.traverse(t)
        }
      }.traverse(tree);
      hasTry
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
          case JUMP(b) if (b != block) => Some(b);
          case _ => None
        }
        if (block.size == 1 && optCont != None) {
          val Some(cont) = optCont;
          val pred = block.predecessors;
          log("Preds: " + pred + " of " + block + " (" + optCont + ")");
          pred foreach { p =>
            p.lastInstruction match {
              case CJUMP(succ, fail, cond, kind) =>
                if (settings.debug.value)
                  log("Pruning empty if branch.");
                changed = true
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

              case CZJUMP(succ, fail, cond, kind) =>
                if (settings.debug.value)
                  log("Pruning empty ifz branch.");
                changed = true
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

              case JUMP(b) =>
                if (settings.debug.value)
                  log("Pruning empty JMP branch.");
                changed = true
                val replaced = p.replaceInstruction(p.lastInstruction, JUMP(cont))
                if (settings.debug.value)
                  assert(replaced, "Didn't find p.lastInstruction")

              case SWITCH(tags, labels) =>
                if (settings.debug.value)
                  log("Pruning empty SWITCH branch.");
                changed = true
                p.replaceInstruction(p.lastInstruction,
                                     SWITCH(tags, labels map (l => if (l == block) cont else l)))
            }
          }
          if (changed) {
            log("Removing block: " + block)
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
        method.code traverse prune0
      } while (changed)

      if (settings.debug.value)
        log("Prune fixpoint reached in " + n + " iterations.");
    }

    def getMaxType(ts: List[Type]): TypeKind = {
      def maxType(a: TypeKind, b: TypeKind): TypeKind =
        a maxType b;

      val kinds = ts map toTypeKind
      kinds reduceLeft maxType
    }

    def isLoopHeaderLabel(name: Name): Boolean =
      name.startsWith("while$") || name.startsWith("doWhile$")

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
    class DuplicateLabels(boundLabels: collection.Set[Symbol]) extends Transformer {
      val labels: Map[Symbol, Symbol] = new HashMap
      var method: Symbol = _
      var ctx: Context = _

      def apply(ctx: Context, t: Tree) = {
        this.method = ctx.method.symbol
        this.ctx = ctx
        transform(t)
      }

      override def transform(t: Tree): Tree = {
        t match {
          case t @ Apply(fun, args) if (t.symbol.isLabel && !boundLabels(t.symbol)) =>
            if (!labels.isDefinedAt(t.symbol)) {
              val oldLabel = t.symbol
              val sym = method.newLabel(oldLabel.pos, unit.fresh.newName(oldLabel.pos, oldLabel.name.toString))
              sym.setInfo(oldLabel.tpe)
              labels(oldLabel) = sym
            }
            val tree = Apply(global.gen.mkAttributedRef(labels(t.symbol)), transformTrees(args)).setPos(t.pos)
            tree.tpe = t.tpe
            tree

          case t @ LabelDef(name, params, rhs) =>
            val name1 = unit.fresh.newName(t.pos, name.toString)
            if (!labels.isDefinedAt(t.symbol)) {
              val oldLabel = t.symbol
              val sym = method.newLabel(oldLabel.pos, name1)
              sym.setInfo(oldLabel.tpe)
              labels(oldLabel) = sym
            }
            val tree = treeCopy.LabelDef(t, name1, params, transform(rhs))
            tree.symbol = labels(t.symbol)

            ctx.labels += (tree.symbol -> (new Label(tree.symbol) setParams(params map (_.symbol))));
            ctx.method.addLocals(params map (p => new Local(p.symbol, toTypeKind(p.symbol.info), false)));

            tree

          case _ => super.transform(t)
        }
      }
    }

    /////////////////////// Context ////////////////////////////////

    abstract class Cleanup;
    case class MonitorRelease(m: Local) extends Cleanup {
      override def equals(other: Any) = m == other;
    }
    case class Finalizer(f: Tree) extends Cleanup {
      override def equals(other: Any) = f == other;
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
      var labels: HashMap[Symbol, Label] = new HashMap()

      /** Current method definition. */
      var defdef: DefDef = _

      /** current exception handlers */
      var handlers: List[ExceptionHandler] = Nil

      /** The current monitors or finalizers, to be cleaned up upon `return'. */
      var cleanups: List[Cleanup] = Nil

      /** The current exception handler, when we generate code for one. */
      var currentExceptionHandler: Option[ExceptionHandler] = None

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
        this.currentExceptionHandler = other.currentExceptionHandler
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
        assert(cleanups.head == monitor,
               "Bad nesting of cleanup operations: " + cleanups + " trying to exit from monitor: " + monitor)
        cleanups = cleanups.tail
        this
      }

      def addFinalizer(f: Tree): this.type = {
        cleanups = Finalizer(f) :: cleanups;
        this
      }

      def removeFinalizer(f: Tree): this.type = {
        assert(cleanups.head == f,
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
        ctx1.labels = new HashMap()
        ctx1.method.code = new Code(m.symbol.simpleName.toString(), m)
        ctx1.bb = ctx1.method.code.startBlock
        ctx1.defdef = d
        ctx1.scope = EmptyScope
        ctx1.enterScope
        ctx1
      }

      /** Return a new context for a new basic block. */
      def newBlock: Context = {
        val block = method.code.newBlock
        handlers foreach (h => h addCoveredBlock block)
        currentExceptionHandler match {
          case Some(e) => e.addBlock(block)
          case None    => ()
        }
        block.varsInScope = new HashSet()
        block.varsInScope ++= scope.varsInScope
        new Context(this) setBasicBlock block
      }

      def enterScope = {
        scope = new Scope(scope)
      }

      def exitScope = {
        if (bb.size > 0) {
          scope.locals foreach { lv => bb.emit(SCOPE_EXIT(lv)) }
        }
        scope = scope.outer
      }

      /** Create a new exception handler and adds it in the list
       * of current exception handlers. All new blocks will be
       * 'covered' by this exception handler (in addition to the
       * previously active handlers).
       */
      def newHandler(cls: Symbol, resultKind: TypeKind): ExceptionHandler = {
        handlerCount += 1
        val exh = new ExceptionHandler(method, "" + handlerCount, cls)
        exh.resultKind = resultKind
        method.addHandler(exh)
        handlers = exh :: handlers
        if (settings.debug.value)
          log("added handler: " + exh);

        exh
      }

      /** Add an active exception handler in this context. It will cover all new basic blocks
       *  created from now on. */
      private def addActiveHandler(exh: ExceptionHandler) {
        handlerCount += 1
        handlers = exh :: handlers
        if (settings.debug.value)
          log("added handler: " + exh);
      }

      /** Return a new context for generating code for the given
       * exception handler.
       */
      def enterHandler(exh: ExceptionHandler): Context = {
        currentExceptionHandler = Some(exh)
        val ctx = newBlock
        exh.setStartBlock(ctx.bb)
        ctx
      }

      /** Remove the given handler from the list of active exception handlers. */
      def removeHandler(exh: ExceptionHandler): Unit = {
        assert(handlerCount > 0 && handlers.head == exh,
               "Wrong nesting of exception handlers." + this + " for " + exh)
        handlerCount -= 1
        handlers = handlers.tail
        if (settings.debug.value)
          log("removed handler: " + exh);

      }

      /** Clone the current context */
      def dup: Context = new Context(this)

      /** Make a fresh local variable. It ensures the 'name' is unique. */
      def makeLocal(pos: Position, tpe: Type, name: String): Local = {
        val sym = method.symbol.newVariable(pos, unit.fresh.newName(pos, name))
          .setInfo(tpe)
          .setFlag(Flags.SYNTHETIC)
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
       * }, (definitions.ThrowableClass,
       *   ctx => {
       *     ctx.bb.emit(...); // exception handler
       *   }), (AnotherExceptionClass,
       *   ctx => {...
       *   } ))</code>
       */
      def Try(body: Context => Context,
              handlers: List[(Symbol, TypeKind, (Context => Context))],
              finalizer: Tree) = {
        val outerCtx = this.dup       // context for generating exception handlers, covered by finalizer
        val finalizerCtx = this.dup   // context for generating finalizer handler
        val afterCtx = outerCtx.newBlock

        val finalizerExh = if (finalizer != EmptyTree) Some({
          val exh = outerCtx.newHandler(NoSymbol, toTypeKind(finalizer.tpe)) // finalizer covers exception handlers
          this.addActiveHandler(exh)  // .. and body aswell
          val ctx = finalizerCtx.enterHandler(exh)
          val exception = ctx.makeLocal(finalizer.pos, definitions.ThrowableClass.tpe, "exc")
          if (settings.Xdce.value) ctx.bb.emit(LOAD_EXCEPTION())
          ctx.bb.emit(STORE_LOCAL(exception));
          val ctx1 = genLoad(finalizer, ctx, UNIT);
          ctx1.bb.emit(LOAD_LOCAL(exception));
          ctx1.bb.emit(THROW());
          ctx1.bb.enterIgnoreMode;
          ctx1.bb.close
          exh
        }) else None

        val exhs = handlers.map { handler =>
            val exh = this.newHandler(handler._1, handler._2)
            var ctx1 = outerCtx.enterHandler(exh)
            if (settings.Xdce.value) ctx1.bb.emit(LOAD_EXCEPTION())
            ctx1 = handler._3(ctx1)
            ctx1.bb.emit(JUMP(afterCtx.bb))
            ctx1.bb.close
            exh
          }
        val bodyCtx = this.newBlock
        if (finalizer != EmptyTree)
          bodyCtx.addFinalizer(finalizer)

        val finalCtx = body(bodyCtx)

        outerCtx.bb.emit(JUMP(bodyCtx.bb))
        outerCtx.bb.close

        exhs.reverse foreach finalCtx.removeHandler
        if (finalizer != EmptyTree) {
          finalCtx.removeFinalizer(finalizer)
        }

        finalCtx.bb.emit(JUMP(afterCtx.bb))
        finalCtx.bb.close

        afterCtx
      }
    }
  }

    /**
     * Represent a label in the current method code. In order
     * to support forward jumps, labels can be created without
     * having a deisgnated target block. They can later be attached
     * by calling `anchor'.
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
        def substMap: Map[Instruction, Instruction] = {
          val map = new HashMap[Instruction, Instruction]()

          toPatch foreach (i => map += (i -> patch(i)))
          map
        }

        val map = substMap
        code traverse (_.subst(map))
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
     * Pseudo jump: it takes a Label instead of a basick block.
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

    def add(l: Local) =
      locals += l

    def remove(l: Local) =
      locals -= l

    /** Return all locals that are in scope. */
    def varsInScope: Buffer[Local] = outer.varsInScope.clone() ++ locals

    override def toString() =
      outer.toString() + locals.mkString("[", ", ", "]")
  }

  object EmptyScope extends Scope(null) {
    override def toString() = "[]"
    override def varsInScope: Buffer[Local] = new ListBuffer
  }

}
