/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.icode;

import scala.collection.mutable.{Map, HashMap};

abstract class GenICode extends SubComponent  {
  import global._;
  import opcodes._;

  val phaseName = "genicode";

  override def newPhase(prev: Phase) = new ICodePhase(prev);

  class ICodePhase(prev: Phase) extends StdPhase(prev) {
    import Primitives._;

    override def description = "Generate ICode from the AST";

    var unit: CompilationUnit = _;

    /** A map from scala primitive Types to ICode TypeKinds */
    private var primitiveTypeMap: Map[Symbol, TypeKind] = null;

    private var primitiveMethods: Map[Symbol, Primitive] = null;

    initPrimitiveTypeMap;

    override def apply(unit: CompilationUnit): Unit = {
      this.unit = unit;
      gen(unit.body);
    }

    def gen(tree: Tree): Context = gen(tree, new Context());

    def gen(trees: List[Tree], ctx: Context): Context = {
      var ctx1 = ctx;
      for (val t <- trees)
        ctx1 = gen(t, ctx1);

      ctx1
    }

    def gen(tree: Tree, ctx: Context): Context = tree match {
      case EmptyTree => ctx;

      case PackageDef(name, stats) => gen(stats, ctx setPackage name);

      case ClassDef(mods, name, tparams, tpt, impl) =>
        ctx setClass (new IClass(tree.symbol) setCompilationUnit unit);
        addClassFields(ctx, tree.symbol);
        classes = ctx.clazz :: classes;
        gen(impl, ctx);
        ctx setClass null;

      // !! modules should be eliminated by refcheck... or not?
      case ModuleDef(mods, name, impl) =>
        ctx setClass (new IClass(tree.symbol) setCompilationUnit unit);
        classes = ctx.clazz :: classes;
        gen(impl, ctx);
        ctx setClass null;

      case ValDef(mods, name, tpt, rhs) => ctx; // we use the symbol to add fields

      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        log("Entering method " + name);
        val m = new IMethod(tree.symbol);
        ctx.clazz.addMethod(m);
        var ctx1 = ctx.enterMethod(m, tree.asInstanceOf[DefDef]);
        addMethodParams(ctx1, vparamss);
        ctx1 = genLoad(rhs,
                       ctx1,
                       toTypeKind(ctx1.method.symbol.info.resultType));
        ctx1.bb.emit(RETURN());
        ctx1;

      case Template(parents, body) =>
        gen(body, ctx);

      case Block(stats, expr) =>
        abort("Block is not allowed in gen " + tree);


/*  case AbsTypeDef(mods, name, lo, hi) =>                          (eliminated by erasure)
  case AliasTypeDef(mods, name, tparams, rhs) =>                  (eliminated by erasure)
  case Import(expr, selectors) =>                                 (eliminated by typecheck)
  case Attributed(attribute, definition) =>                       (eliminated by typecheck)
  case DocDef(comment, definition) =>                             (eliminated by typecheck)
  case CaseDef(pat, guard, body) =>                               (eliminated by transmatch)
  case Sequence(trees) =>                                         (eliminated by transmatch)
  case Alternative(trees) =>                                      (eliminated by transmatch)
  case Star(elem) =>                                              (eliminated by transmatch)
  case Bind(name, body) =>                                        (eliminated by transmatch)
  case ArrayValue(elemtpt, trees) =>                              (introduced by uncurry)
  case Function(vparams, body) =>                                 (eliminated by typecheck)
  case Typed(expr, tpt) =>                                         (eliminated by erasure)
  case TypeApply(fun, args) =>
  case SingletonTypeTree(ref) =>                                  (eliminated by typecheck)
  case SelectFromTypeTree(qualifier, selector) =>                 (eliminated by typecheck)
  case CompoundTypeTree(templ: Template) =>                        (eliminated by typecheck)
  case AppliedTypeTree(tpt, args) =>                               (eliminated by typecheck)
*/
    }

    private def genStat(trees: List[Tree], ctx: Context): Context = {
      var currentCtx = ctx;

      for (val t <- trees)
        currentCtx = genStat(t, currentCtx);

      currentCtx
    }

    /**
     * Generate code for the given tree. The trees should contain statements
     * and not produce any value. Use genLoad for expressions which leave
     * a value on top of the stack.
     *
     * @return a new context. This is necessary for control flow instructions
     * which may change the current basic block.
     */
    private def genStat(tree: Tree, ctx: Context): Context = tree match {
      case Assign(lhs @ Select(_, _), rhs) =>
        if (lhs.symbol.isStatic) {
          val ctx1 = genLoad(rhs, ctx, toTypeKind(lhs.symbol.info));
          ctx1.bb.emit(STORE_FIELD(lhs.symbol, true));
          ctx1
        } else {
          var ctx1 = genLoadQualifier(lhs, ctx);
          ctx1 = genLoad(rhs, ctx1, toTypeKind(lhs.symbol.info));
          ctx1.bb.emit(STORE_FIELD(lhs.symbol, false));
          ctx1
        }

        case Assign(lhs, rhs) =>
          assert(ctx.method.locals.contains(lhs.symbol),
                 "Assignment to inexistent local: " + lhs.symbol);
          val ctx1 = genLoad(rhs, ctx, toTypeKind(lhs.symbol.info));
          ctx1.bb.emit(STORE_LOCAL(lhs.symbol, lhs.symbol.isValueParameter));
          ctx1

        case _ =>
          log("Passing " + tree + " to genLoad");
          genLoad(tree, ctx, UNIT);
    }

    private def genLoad(tree: Tree, ctx: Context, expectedType: TypeKind): Context = {
      tree match {
        case LabelDef(name, params, rhs) =>
          ctx.method.addLocals(params map (.symbol));
          val ctx1 = ctx.newBlock;
          ctx1.labels.get(tree.symbol) match {
            case Some(label) => label.anchor(ctx1.bb);

            case None =>
              ctx.labels += tree.symbol -> (new Label(tree.symbol) anchor ctx1.bb setParams (params map (.symbol)));
          }
          genLoad(rhs, ctx1, toTypeKind(tree.symbol.info.resultType));

        case ValDef(_, _, _, rhs) =>
          val sym = tree.symbol;
          ctx.method.addLocal(sym);
          val ctx1 = genLoad(rhs, ctx, toTypeKind(sym.info));
          ctx1.bb.emit(STORE_LOCAL(sym, false));
          ctx1

        case If(cond, thenp, elsep) =>
          var thenCtx = ctx.newBlock;
          var elseCtx = ctx.newBlock;
          val contCtx = ctx.newBlock;
          genCond(cond, ctx, thenCtx, elseCtx);
          thenCtx = genLoad(thenp, thenCtx, toTypeKind(tree.tpe));
          elseCtx = genLoad(elsep, elseCtx, toTypeKind(tree.tpe));
          thenCtx.bb.emit(JUMP(contCtx.bb));
          elseCtx.bb.emit(JUMP(contCtx.bb));
          contCtx;

        case Return(expr) =>
          val ctx1 = genLoad(expr, ctx, expectedType);
          ctx1.bb.emit(RETURN());
          ctx1

        case Try(block, catches, finalizer) =>
          abort("Try blocks not handled yet");

        case Throw(expr) =>
          val ctx1 = genLoad(expr, ctx, expectedType);
          ctx1.bb.emit(THROW());
          ctx;

        case New(tpt) =>
          ctx.bb.emit(NEW(tree.symbol));
          ctx;

        case Apply(TypeApply(fun, targs), _) =>
          val sym = fun.symbol;
          val ctx1 = genLoadQualifier(fun, ctx);

          if (sym == definitions.Any_isInstanceOfErased)
            ctx1.bb.emit(IS_INSTANCE(targs.head.tpe));
          else if (sym == definitions.Any_asInstanceOfErased)
            ctx1.bb.emit(CHECK_CAST(targs.head.tpe));
          else
            abort("Unexpected type application");
          ctx1

        case Apply(fun @ Select(Super(_, mixin), _), args) =>
          val invokeStyle = SuperCall(mixin);
          val ctx1 = genLoadArguments(args, fun.symbol.info.paramTypes, ctx);

          ctx1.bb.emit(CALL_METHOD(fun.symbol, invokeStyle));
          ctx1


        case Apply(fun, args) =>
          val sym = fun.symbol;
          if (sym.isLabel) {

            val label = ctx.labels.get(sym) match {
              case Some(l) => l;

              // it is a forward jump, scan for labels
              case None =>
                scanForLabels(ctx.defdef, ctx);
                ctx.labels.get(sym) match {
                  case Some(l) => l;
                  case _       => abort("Unknown label target: " + sym);
                }
            }
            val ctx1 = genLoadLabelArguments(args, label, ctx);
            if (label.anchored)
              ctx1.bb.emit(JUMP(label.block));
            else
              ctx1.bb.emit(PJUMP(label));

            ctx1.bb.close;
            ctx1.newBlock;
          } else if (isPrimitive(fun.symbol)) {

            abort("Primitive operations not handled yet");

          } else {
            // normal method call

            var invokeStyle =
              if (sym.isClassConstructor)
                NewInstance;
              else if (sym.isStatic)
                Static(false)
              else
                Dynamic;

            var ctx1 = genLoadQualifier(fun, ctx);
            ctx1 = genLoadArguments(args, fun.symbol.info.paramTypes, ctx1);

            ctx1.bb.emit(CALL_METHOD(sym, invokeStyle));
            ctx1
          }

        case This(qual) =>
          assert(tree.symbol == ctx.clazz.symbol,
                 "Trying to access the this of another class: " +
                 "tree.symbol = " + tree.symbol + ", ctx.clazz.symbol = " + ctx.clazz.symbol);
          ctx.bb.emit(THIS(ctx.clazz.symbol));
          ctx;

        case Select(qualifier, selector) =>
          val sym = tree.symbol;
          if (sym.isStatic) {
            ctx.bb.emit(LOAD_FIELD(sym, true));
            ctx
          } else {
            val ctx1 = genLoadQualifier(tree, ctx); // !! attention
            ctx1.bb.emit(LOAD_FIELD(sym, false));
            ctx1
          }

        case Ident(name) =>
          if (tree.symbol.isModule)
            abort("Modules are not handled yet");
          else
            ctx.bb.emit(LOAD_LOCAL(tree.symbol, tree.symbol.isValueParameter ));
          ctx

        case Literal(value) =>
          ctx.bb.emit(CONSTANT(value));
          ctx

        case Block(stats, expr) =>
          log("Entering block");
          assert(!(ctx.method eq null), "Block outside method");
          val ctx1 = genStat(stats, ctx);
          genLoad(expr, ctx1, expectedType);

        case EmptyTree => ctx;

        case Assign(_, _) => genStat(tree, ctx);

        case _ => abort("Unexpected tree in genLoad: " + tree);
      }
    }

    /** Load the qualifier of `tree' on top of the stack. */
    private def genLoadQualifier(tree: Tree, ctx: Context): Context =
      tree match {
        case Select(qualifier, _) =>
          genLoad(qualifier, ctx, REF);
        case _ =>
          abort("Unknown qualifier " + tree);
      }

    /**
     * Generate code that loads args into label parameters.
     */
    private def genLoadLabelArguments(args: List[Tree], label: Label, ctx: Context): Context = {
      assert(args.length == label.params.length,
             "Wrong number of arguments in call to label " + label.symbol);
      var ctx1 = ctx;
      var arg = args;
      var param = label.params;

      while (arg != Nil) {
        ctx1 = genLoad(arg.head, ctx1, toTypeKind(param.head.info));
        ctx1.bb.emit(STORE_LOCAL(param.head, param.head.isValueParameter));
        arg = arg.tail;
        param = param.tail;
      }
      ctx1
    }

    private def genLoadArguments(args: List[Tree], tpes: List[Type], ctx: Context): Context = {
      assert(args.length == tpes.length, "Wrong number of arguments in call " + ctx);

      var ctx1 = ctx;
      var arg = args;
      var tpe = tpes;
      while (arg != Nil) {
        ctx1 = genLoad(arg.head, ctx1, toTypeKind(tpe.head));
        arg = arg.tail;
        tpe = tpe.tail;
      }
      ctx1
    }

    /** Is the given symbol a primitive operation? */
    def isPrimitive(fun: Symbol): Boolean = false;

    /**
     * Traverse the tree and store label stubs in the contxt. This is
     * necessary to handle forward jumps, because at a label application
     * with arguments, the symbols of the corresponding LabelDef parameters
     * are not yet known.
     *
     * Attention! Might be expensive to traverse each method twice! An
     * optimization would be to make it lazily, only if forward jumps
     * really happen.
     */
    private def scanForLabels(tree: Tree, ctx: Context): Unit =
      new Traverser() {
        override def traverse(tree: Tree): Unit = tree match {

          case LabelDef(name, params, _) =>
            ctx.labels += tree.symbol -> (new Label(tree.symbol) setParams(params map (.symbol)));

          case _ => super.traverse(tree);
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
                        elseCtx: Context): Unit = {


    }

    /**
     * Add all fields of the given class symbol to the current ICode
     * class.
     */
    private def addClassFields(ctx: Context, cls: Symbol): Unit = {
      assert(ctx.clazz.symbol eq cls,
             "Classes are not the same: " + ctx.clazz.symbol + ", " + cls);

      for (val f <- cls.info.decls.elements)
        if (!f.isMethod && f.isTerm)
          ctx.clazz.addField(new IField(f));
    }

    /**
     * Add parameters to the current ICode method. It is assumed the methods
     * have been uncurried, so the list of lists contains just one list.
     */
    private def addMethodParams(ctx: Context, vparamss: List[List[ValDef]]): Unit =
      vparamss match {
        case Nil => ()

        case vparams :: Nil =>
          for (val p <- vparams)
            ctx.method.addParam(p.symbol);

        case _ =>
          abort("Malformed parameter list: " + vparamss);
      }

    def toTypeKind(t: Type): TypeKind = t match {
      case ThisType(_) => REF;

      case SingleType(pre, sym) =>
        primitiveTypeMap get sym match {
          case Some(k) => k;
          case None    => REF;
        }

      case ConstantType(value) =>
        toTypeKind(value.tpe);

      case TypeRef(_, sym, _) => REF;
      case _ => abort("Unknown type");
    }

    /** Initialize the map from scala primitive types to ICode types */
    private def initPrimitiveTypeMap = {
      log("Initializing primitive map");
      primitiveTypeMap = new HashMap();
      primitiveTypeMap += definitions.UnitClass -> UNIT;
      primitiveTypeMap += definitions.BooleanClass -> BOOL;
      primitiveTypeMap += definitions.ByteClass -> I1;
      primitiveTypeMap += definitions.ShortClass -> I2;
      primitiveTypeMap += definitions.CharClass -> I2;
      primitiveTypeMap += definitions.IntClass -> I4;
      primitiveTypeMap += definitions.LongClass -> I8;
      primitiveTypeMap += definitions.FloatClass -> R4;
      primitiveTypeMap += definitions.DoubleClass -> R8;
      primitiveTypeMap += definitions.StringClass -> STRING;
    }

    /////////////////////// Context ////////////////////////////////


    /**
     * The Context class keeps information relative to the current state
     * in code generation
     */
    class Context {

      /** The current package. */
      var packg: Name = _;

      /** The current class. */
      var clazz: IClass = _;

      /** The current method. */
      var method: IMethod = _;

      /** The current basic block. */
      var bb: BasicBlock = _;

      /** Map from label symbols to label objects. */
      var labels: HashMap[Symbol, Label] = new HashMap();

      /** Current method definition. */
      var defdef: DefDef = _;

      /** TODO: add a current exception handler */

      def this(other: Context) = {
        this();
        this.packg = other.packg;
        this.clazz = other.clazz;
        this.method = other.method;
        this.bb = other.bb;
        this.labels = other.labels;
        this.defdef = other.defdef;
      }

      def setPackage(p: Name): this.type = {
        this.packg = p;
        this
      }

      def setClass(c: IClass): this.type = {
        this.clazz = c;
        this
      }

      def setMethod(m: IMethod): this.type = {
        this.method = m;
        this
      }

      def setBasicBlock(b: BasicBlock): this.type = {
        this.bb = b;
        this
      }

      /** Prepare a new context upon entry into a method */
      def enterMethod(m: IMethod, d: DefDef): Context = {
        val ctx1 = new Context(this) setMethod(m);
        ctx1.labels = new HashMap();
        ctx1.method.code = new Code(m.symbol.simpleName.toString());
        ctx1.bb = ctx1.method.code.startBlock;
        ctx1.defdef = d;
        ctx1
      }

      /** Return a new context for a new basic block. */
      def newBlock: Context =
        new Context(this) setBasicBlock (method.code.newBlock);
    }

    /**
     * Represent a label in the current method code. In order
     * to support forward jumps, labels can be created without
     * having a deisgnated target block. They can later be attached
     * by calling `anchor'.
     */
    class Label(val symbol: Symbol) {
      var anchored = false;
      var block: BasicBlock = _;
      var params: List[Symbol] = _;

      private var toPatch: List[Instruction] = Nil;

      /** Fix this label to the given basic block. */
      def anchor(b: BasicBlock): Label = {
        assert(!anchored, "Cannot anchor an already anchored label!");
        this.block = b;
        this
      }

      def setParams(p: List[Symbol]): Label = {
        assert(params == null, "Cannot set label parameters twice!");
        params = p;
        this
      }

      /** Add an instruction that refers to this label. */
      def addCallingInstruction(i: Instruction) =
        toPatch = i :: toPatch;

      /**
       * Patch the code by replacing pseudo call instructions with
       * jumps to the given basic block.
       */
      def patch(code: Code): Unit = {
        def substMap: Map[Instruction, Instruction] = {
          val map = new HashMap[Instruction, Instruction]();

          toPatch foreach (i => map += i -> patch(i));
          map
        }

        val map = substMap;
        code traverse (.subst(map));
      }

      /**
       * Return the patched instruction. If the given instruction
       * jumps to this label, replace it with the basic block. Otherwise,
       * return the same instruction. Conditional jumps have more than one
       * label, so they are replaced only if all labels are anchored.
       */
      def patch(instr: Instruction): Instruction = {
        assert(anchored, "Cannot patch until this label is anchored: " + this);

        instr match {
          case PJUMP(self)
          if (self == this) => JUMP(block);

          case PCJUMP(self, failure, cond)
          if (self == this && failure.anchored) =>
            CJUMP(block, failure.block, cond);

          case PCJUMP(success, self, cond)
          if (self == this && success.anchored) =>
            CJUMP(success.block, block, cond);

          case PCZJUMP(self, failure, cond)
          if (self == this && failure.anchored) =>
            CZJUMP(block, failure.block, cond);

          case PCZJUMP(success, self, cond)
          if (self == this && success.anchored) =>
            CZJUMP(success.block, block, cond);

          case _ => instr;
        }
      }
    }

    ///////////////// Fake instructions //////////////////////////

    /**
     * Pseudo jump: it takes a Label instead of a basick block.
     * It is used temporarily during code generation. It is replaced
     * by a real JUMP instruction when all labels are resolved.
     */
    class PseudoJUMP(label: Label) extends Instruction {
      override def toString(): String ="PJUMP " + label.symbol.simpleName;

      override def consumed = 0;
      override def produced = 0;

      // register with the given label
      if (!label.anchored)
        label.addCallingInstruction(this);
    }

    case class PJUMP(where: Label) extends PseudoJUMP(where);

    case class PCJUMP(success: Label, failure: Label, cond: TestOp)
         extends PseudoJUMP(success) {

       if (!failure.anchored)
         failure.addCallingInstruction(this);
    }

    case class PCZJUMP(success: Label, failure: Label, cond: TestOp)
         extends PseudoJUMP(success) {

       if (!failure.anchored)
         failure.addCallingInstruction(this);
    }

  }
}

