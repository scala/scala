/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.jvm;

import scala.collection.mutable.{Map, HashMap};
import scala.tools.nsc.symtab._;

import ch.epfl.lamp.fjbg._;

/**
 */
abstract class BytecodeGenerators extends SubComponent {
  import global._;
  import icodes._;
  import icodes.opcodes._;

  val phaseName = "jvm";

  /** Create a new phase */
  override def newPhase(p: Phase) = new JvmPhase(p);

  /** JVM code generation phase */
  class JvmPhase(prev: Phase) extends GlobalPhase(prev) {
    def name = phaseName;
    override def newFlags = phaseNewFlags;

    val codeGenerator = new GenJVM;

    override def run: Unit =
      classes foreach codeGenerator.genClass;

    override def apply(unit: CompilationUnit): Unit =
      abort("JVM works on icode classes, not on compilation units!");
  }

  /**
   * Java bytecode generator
   */
  class GenJVM {
    val MIN_SWITCH_DENSITY = 0.7;

    var clasz: IClass = _;
    var method: IMethod = _;
    var code: Code = _;
    var jclass: JClass = _;
    var jmethod: JMethod = _;
    var jcode: JExtendedCode = _;

    val fjbgContext = new FJBGContext();

    def genClass(c: IClass): Unit = {
      log("Generating class " + c);
      clasz = c;
      var parents = c.symbol.info.parents;
      var ifaces = JClass.NO_INTERFACES;

      if (parents.isEmpty)
        parents = definitions.ObjectClass.tpe :: parents;

      if (parents.length > 1 ) {
        ifaces = new Array[String](parents.length - 1);
        parents.drop(1).map((s) => javaName(s.symbol)).copyToArray(ifaces, 0);
        ()
      }

      jclass = fjbgContext.JClass(javaFlags(c.symbol),
                                  javaName(c.symbol),
                                  javaName(parents(0).symbol),
                                  ifaces,
                                  c.cunit.source.toString());

      clasz.fields foreach genField;
      clasz.methods foreach genMethod;

      jclass.writeTo(getFile(c.symbol, ".class"));
    }

    def genField(f: IField): Unit  = {
      log("Adding field: " + f.symbol.fullNameString);
      jclass.addNewField(javaFlags(f.symbol),
                         javaName(f.symbol),
                         javaType(toTypeKind(f.symbol.tpe)));
    }

    def genMethod(m: IMethod): Unit = {
      log("Adding method " + m.symbol.fullNameString + " ctor: " + m.symbol.isClassConstructor);
      method = m;

      var resTpe = javaType(toTypeKind(m.symbol.tpe.resultType));
      if (m.symbol.isClassConstructor)
        resTpe = JType.VOID;

      jmethod = jclass.addNewMethod(javaFlags(m.symbol),
                                    javaName(m.symbol),
                                    resTpe,
                                    javaTypes(m.params map (p => toTypeKind(p.tpe))),
                                    javaNames(m.params));

      if (!jmethod.isAbstract()) {
        jcode = jmethod.getCode().asInstanceOf[JExtendedCode];
        genCode(m.code);
      }
    }


    val linearizer = new NormalLinearizer();

    def genCode(c: Code): Unit = {
      code = c;
      val blocks = linearizer.linearize(code);
      makeLabels(blocks);
      blocks foreach genBlock;
    }

    def genBlock(b: BasicBlock): Unit = {
      labels(b).anchorToNext();

      b traverse ( instr => {
        instr match {
          case THIS(clasz) =>
            jcode.emitALOAD_0();

          case CONSTANT(const) =>
            const.tag match {
              case UnitTag => ();
              case BooleanTag => jcode.emitPUSH(const.booleanValue);
              case ByteTag => jcode.emitPUSH(const.byteValue);
              case ShortTag => jcode.emitPUSH(const.shortValue);
              case IntTag => jcode.emitPUSH(const.intValue);
              case LongTag => jcode.emitPUSH(const.longValue);
              case FloatTag => jcode.emitPUSH(const.floatValue);
              case DoubleTag => jcode.emitPUSH(const.doubleValue);
              case StringTag => jcode.emitPUSH(const.stringValue);
              case NullTag => jcode.emitACONST_NULL();
              case _ => abort("Unknown constant value: " + const);
            }

          case LOAD_ARRAY_ITEM(kind) =>
            jcode.emitALOAD(javaType(kind));

          case LOAD_LOCAL(local, isArg) =>
            if (isArg)
              jcode.emitLOAD(1 + method.params.indexOf(local), javaType(local));
            else
              jcode.emitLOAD(1 + method.locals.indexOf(local), javaType(local));

          case LOAD_FIELD(field, isStatic) =>
            if (isStatic)
              jcode.emitGETSTATIC(javaName(field.owner),
                                  javaName(field),
                                  javaType(field));
            else
              jcode.emitGETFIELD(javaName(field.owner),
                                  javaName(field),
                                  javaType(field));

          case LOAD_MODULE(module) =>
            ();

          case STORE_ARRAY_ITEM(kind) =>
            jcode.emitASTORE(javaType(kind));

          case STORE_LOCAL(local, isArg) =>
            if (isArg)
              jcode.emitSTORE(1 + method.params.indexOf(local), javaType(local));
            else
              jcode.emitSTORE(1 + method.locals.indexOf(local), javaType(local));

          case STORE_FIELD(field, isStatic) =>
            if (isStatic)
              jcode.emitPUTSTATIC(javaName(field.owner),
                                  javaName(field),
                                  javaType(field));
            else
              jcode.emitPUTFIELD(javaName(field.owner),
                                  javaName(field),
                                  javaType(field));

          case CALL_PRIMITIVE(primitive) =>
            genPrimitive(primitive);

          // TODO: reference the type of the receiver instead of the
          // method owner.
          case CALL_METHOD(method, style) =>
            style match {
              case Dynamic =>
                if (method.owner.hasFlag(Flags.INTERFACE))
                  jcode.emitINVOKEINTERFACE(javaName(method.owner),
                                            javaName(method),
                                            javaType(method).asInstanceOf[JMethodType])
                else
                  jcode.emitINVOKEVIRTUAL(javaName(method.owner),
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);

              case Static(instance) =>
                if (instance) {
                  jcode.emitINVOKESPECIAL(javaName(method.owner),
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);
                } else
                  jcode.emitINVOKESTATIC(javaName(method.owner),
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);

              case SuperCall(_) =>
                  jcode.emitINVOKESPECIAL(javaName(method.owner),
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);
            }

          case NEW(ctor) =>
            val className = javaName(ctor.owner);
            jcode.emitNEW(className);
            jcode.emitDUP();
            jcode.emitINVOKESPECIAL(className,
                                    JMethod.INSTANCE_CONSTRUCTOR_NAME,
                                    javaType(ctor).asInstanceOf[JMethodType]);

          case CREATE_ARRAY(elem) =>
            jcode.emitNEWARRAY(javaType(elem));

          case IS_INSTANCE(tpe) =>
            tpe match {
              case REFERENCE(cls) => jcode.emitINSTANCEOF(new JObjectType(javaName(cls)));
              case ARRAY(elem)    => jcode.emitINSTANCEOF(new JArrayType(javaType(elem)));
              case _ => abort("Unknown reference type in IS_INSTANCE: " + tpe);
            }

          case CHECK_CAST(tpe) =>
            tpe match {
              case REFERENCE(cls) => jcode.emitCHECKCAST(new JObjectType(javaName(cls)));
              case ARRAY(elem)    => jcode.emitCHECKCAST(new JArrayType(javaType(elem)));
              case _ => abort("Unknown reference type in IS_INSTANCE: " + tpe);
            }

          case SWITCH(tags, branches) =>
            val tagArray = new Array[Array[Int]](tags.length);
            var caze = tags;
            var i = 0;
            while (i < tagArray.length) {
              tagArray(i) = new Array[Int](caze.head.length);
              caze.head.copyToArray(tagArray(i), 0);
              i = i + 1;
              caze = caze.tail;
            }
            val branchArray = new Array[JCode$Label](tagArray.length);
            jcode.emitSWITCH(tagArray,
                             (branches map labels dropRight 1).copyToArray(branchArray, 0),
                             labels(branches.last),
                             MIN_SWITCH_DENSITY);

          case JUMP(where) =>
            jcode.emitGOTO_maybe_W(labels(where), false); // default to short jumps

          case CJUMP(success, failure, cond, kind) =>
            kind match {
              case BOOL | BYTE | CHAR | SHORT | INT =>
                jcode.emitIF_ICMP(conds(cond), labels(success));
                jcode.emitGOTO_maybe_W(labels(failure), false);

              case REFERENCE(_) | ARRAY(_) =>
                jcode.emitIF_ACMP(conds(cond), labels(success));
                jcode.emitGOTO_maybe_W(labels(failure), false);

              case _ =>
                kind match {
                  case LONG   => jcode.emitLCMP();
                  case FLOAT  => jcode.emitFCMPG();
                  case DOUBLE => jcode.emitDCMPG();
                }
                jcode.emitIF(conds(cond), labels(success));
                jcode.emitGOTO_maybe_W(labels(failure), false);
            }

          case CZJUMP(success, failure, cond, kind) =>
            kind match {
              case BOOL | BYTE | CHAR | SHORT | INT =>
                jcode.emitIF(conds(cond), labels(success));
                jcode.emitGOTO_maybe_W(labels(failure), false);

              case REFERENCE(_) | ARRAY(_) =>
                jcode.emitIFNULL(labels(success));
                jcode.emitGOTO_maybe_W(labels(failure), false);

              case _ =>
                kind match {
                  case LONG   => jcode.emitLCONST_0(); jcode.emitLCMP();
                  case FLOAT  => jcode.emitFCONST_0(); jcode.emitFCMPL();
                  case DOUBLE => jcode.emitDCONST_0(); jcode.emitDCMPL();
                }
                jcode.emitIF(conds(cond), labels(success));
                jcode.emitGOTO_maybe_W(labels(failure), false);
            }

          case RETURN(kind) =>
            jcode.emitRETURN(javaType(kind));

          case THROW() =>
            jcode.emitATHROW();

          case DROP(kind) =>
//             kind match {
//               case LONG | DOUBLE => jcode.emitPOP2();
//               case _ => jcode.emitPOP();
//             }

          case DUP(kind) =>
            kind match {
              case LONG | DOUBLE => jcode.emitDUP2();
              case _ => jcode.emitDUP();
            }


          case MONITOR_ENTER() =>
            jcode.emitMONITORENTER();

          case MONITOR_EXIT() =>
            jcode.emitMONITOREXIT();
        }
      });
    }


    def genPrimitive(primitive: Primitive): Unit = {
      primitive match {
        case Negation(kind) =>
          kind match {
            case BOOL | BYTE | CHAR | SHORT | INT =>
              jcode.emitINEG();

            case LONG   => jcode.emitLNEG();
            case FLOAT  => jcode.emitFNEG();
            case DOUBLE => jcode.emitDNEG();
            case _ => abort("Impossible to negate a " + kind);
          }

        case Arithmetic(op, kind) =>
          op match {
            case ADD => jcode.emitADD(javaType(kind));
            case SUB =>
              kind match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitISUB();
                case LONG   => jcode.emitLSUB();
                case FLOAT  => jcode.emitFSUB();
                case DOUBLE => jcode.emitDSUB();
              }

            case MUL =>
              kind match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitIMUL();
                case LONG   => jcode.emitLMUL();
                case FLOAT  => jcode.emitFMUL();
                case DOUBLE => jcode.emitDMUL();
              }

            case DIV =>
              kind match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitIDIV();
                case LONG   => jcode.emitLDIV();
                case FLOAT  => jcode.emitFDIV();
                case DOUBLE => jcode.emitDDIV();
              }

            case REM =>
              kind match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitIREM();
                case LONG   => jcode.emitLREM();
                case FLOAT  => jcode.emitFREM();
                case DOUBLE => jcode.emitDREM();
              }

            case NOT =>
              kind match {
                case BOOL | BYTE | CHAR | SHORT | INT =>
                  jcode.emitPUSH(-1);
                  jcode.emitIXOR();;
                case LONG   =>
                  jcode.emitPUSH(-1l);
                  jcode.emitLXOR();;
                case _ => abort("Impossible to negate an " + kind);
              }

            case _ => abort("Unknown arithmetic primitive " + primitive );
          }

        case Conversion(src, dst) =>
          jcode.emitT2T(javaType(src), javaType(dst));

        case _ => log("Unimplemented primitive " + primitive);
      }
    }

    val labels: HashMap[BasicBlock, JCode$Label] = new HashMap();
    val conds: HashMap[TestOp, Int] = new HashMap();

    conds += EQ -> JExtendedCode.COND_EQ;
    conds += NE -> JExtendedCode.COND_NE;
    conds += LT -> JExtendedCode.COND_LT;
    conds += GT -> JExtendedCode.COND_GT;
    conds += LE -> JExtendedCode.COND_LE;
    conds += GE -> JExtendedCode.COND_GE;

    def makeLabels(bs: List[BasicBlock]) = {
      labels.clear;
      bs foreach (bb => labels += bb -> jcode.newLabel() );
    }

    ////////////////////// Utilities ////////////////////////

    def javaName(sym: Symbol) =
      if (sym.isClass)
        sym.fullNameString('/')
      else
        sym.simpleName.toString();

    def javaNames(syms: List[Symbol]): Array[String] = {
      val res = new Array[String](syms.length);
      var i = 0;
      syms foreach ( s => { res(i) = javaName(s); i = i + 1; } );
      res
    }

    /**
     * Return the Java modifiers for the given symbol.
     * Java modifiers for classes:
     *  - public, abstract, final, strictfp (not used)
     * for interfaces:
     *  - the same as for classes, without 'final'
     * for fields:
     *  - public, protected, private
     *  - static, final
     * for methods:
     *  - the same as for fields, plus:
     *  - abstract, synchronized (not used), strictfp (not used), native (not used)
     */
    def javaFlags(sym: Symbol): Int = {
      import JAccessFlags._;

      var jf: Int = 0;
      val f = sym.flags;
      jf = jf | (if (sym hasFlag Flags.PRIVATE) ACC_PRIVATE else ACC_PUBLIC);
      jf = jf | (if ((sym hasFlag Flags.ABSTRACT) ||
                     (sym hasFlag Flags.DEFERRED)) ACC_ABSTRACT else 0);
      jf = jf | (if (sym hasFlag Flags.INTERFACE) ACC_INTERFACE else 0);
      jf = jf | (if (sym hasFlag Flags.FINAL) ACC_FINAL else 0);
      jf = jf | (if (isStaticSymbol(sym)) ACC_STATIC else 0);
      jf
    }

    def isStaticSymbol(s: Symbol): Boolean =
      s.hasFlag(Flags.STATIC) || s.hasFlag(Flags.STATICMEMBER) || s.owner.isImplClass;

    def javaType(t: TypeKind): JType = t match {
      case UNIT            => JType.VOID;

      case BOOL            => JType.BOOLEAN;
      case BYTE            => JType.BYTE;
      case SHORT           => JType.SHORT;
      case CHAR            => JType.CHAR;
      case INT             => JType.INT;
      case LONG            => JType.LONG;
      case FLOAT           => JType.FLOAT;
      case DOUBLE          => JType.DOUBLE;
      case REFERENCE(cls)  => new JObjectType(javaName(cls));
      case ARRAY(elem)     => new JArrayType(javaType(elem));
    }

    def javaType(s: Symbol): JType =
      if (s.isMethod)
        new JMethodType(
          if (s.isClassConstructor)
            JType.VOID else javaType(toTypeKind(s.tpe.resultType)),
          javaTypes(s.tpe.paramTypes map toTypeKind))
      else
        javaType(toTypeKind(s.tpe));

    def javaTypes(ts: List[TypeKind]): Array[JType] = {
      val res = new Array[JType](ts.length);
      var i = 0;
      ts foreach ( t => { res(i) = javaType(t); i = i + 1; } );
      res
    }

//     def javaTypes(syms: List[Symbol]): Array[JType] = {
//       val res = new Array[JType](syms.length);
//       var i = 0;
//       syms foreach ( s => { res(i) = javaType(toTypeKind(s.tpe)); i = i + 1; } );
//       res
//     }
  }

}
