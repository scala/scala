/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc.backend.jvm;

import java.io.File;

import scala.collection.mutable.{Map, HashMap};
import scala.tools.nsc.symtab._;
import scala.tools.nsc.util.Position;

import ch.epfl.lamp.fjbg._;

/**
 */
abstract class GenJVM extends SubComponent {
  import global._;
  import icodes._;
  import icodes.opcodes._;

  val phaseName = "jvm";

  /** Create a new phase */
  override def newPhase(p: Phase) = new JvmPhase(p);

  /** JVM code generation phase
   */
  class JvmPhase(prev: Phase) extends GlobalPhase(prev) {
    def name = phaseName;
    override def newFlags = phaseNewFlags;

    override def erasedTypes = true;
    val codeGenerator = new BytecodeGenerator;

    override def run: Unit = {
      if (settings.debug.value) inform("[running phase " + name + " on icode]");
      classes.values foreach codeGenerator.genClass;
    }

    override def apply(unit: CompilationUnit): Unit =
      abort("JVM works on icode classes, not on compilation units!");
  }

  /**
   * Java bytecode generator.
   *
   */
  class BytecodeGenerator {
    val MIN_SWITCH_DENSITY = 0.7;
    val MODULE_INSTANCE_NAME = "MODULE$";
    val JAVA_LANG_STRINGBUFFER = "java.lang.StringBuffer";
    val JAVA_RMI_REMOTEEXCEPTION = "java.rmi.RemoteException";

    val stringBufferType = new JObjectType(JAVA_LANG_STRINGBUFFER);
    val toStringType = new JMethodType(JObjectType.JAVA_LANG_STRING, JType.EMPTY_ARRAY);

    // Scala attributes
    val SerializableAttr = definitions.SerializableAttr.tpe;
    val BeanPropertyAttr = definitions.BeanPropertyAttr.tpe;
    val SerialVersionUID = definitions.getClass("scala.SerialVersionUID").tpe;
    val CloneableAttr    = definitions.getClass("scala.cloneable").tpe;
    val TransientAtt     = definitions.getClass("scala.transient").tpe;
    val VolatileAttr     = definitions.getClass("scala.volatile").tpe;
    val RemoteAttr       = definitions.getClass("scala.remote").tpe;

    val CloneableClass   = definitions.getClass("java.lang.Cloneable");
    val RemoteInterface  = definitions.getClass("java.rmi.Remote");

    var clasz: IClass = _;
    var method: IMethod = _;
    var code: Code = _;
    var jclass: JClass = _;
    var jmethod: JMethod = _;
    var jcode: JExtendedCode = _;

    val fjbgContext = new FJBGContext();

    def emitClass(jclass: JClass, sym: Symbol): Unit = {
      def addScalaAttr(sym: Symbol): Unit = currentRun.symData.get(sym) match {
        case Some(pickle) =>
          val scalaAttr = fjbgContext.JOtherAttribute(jclass,
                                                  jclass,
                                                  nme.ScalaSignatureATTR.toString(),
                                                  pickle.bytes,
                                                  pickle.writeIndex);
          jclass.addAttribute(scalaAttr);
          currentRun.symData -= sym;
          currentRun.symData -= sym.linkedSym;
          //System.out.println("Generated ScalaSig Attr for " + sym);//debug
        case _ =>
          log("Could not find pickle information for " + sym);
      }
      if (!jclass.getName().endsWith("$"))
        addScalaAttr(if (isTopLevelModule(sym)) sym.sourceModule else sym);
      val outfile = getFile(jclass, ".class");
      jclass.writeTo(outfile);
      val file = scala.tools.util.AbstractFile.getFile(outfile);
      informProgress("wrote " + outfile + " " + (if (file != null) "" + file.getFile() + " " + file.getFile().exists() else "no file"));
    }

    var serialVUID: Option[Long] = None;
    var remoteClass: Boolean = false;

    def genClass(c: IClass): Unit = {
      if (settings.debug.value)
        log("Generating class " + c.symbol + " flags: " + Flags.flagsToString(c.symbol.flags));
      clasz = c;
      var parents = c.symbol.info.parents;
      var ifaces  = JClass.NO_INTERFACES;
      val name    = javaName(c.symbol);
      serialVUID  = None;
      remoteClass = false;

      if (parents.isEmpty)
        parents = definitions.ObjectClass.tpe :: parents;

      c.symbol.attributes foreach { a => a match {
          case Pair(SerializableAttr, _) =>
            parents = parents ::: List(definitions.SerializableClass.tpe);
          case Pair(CloneableAttr, _) =>
            parents = parents ::: List(CloneableClass.tpe);
          case Pair(SerialVersionUID, value :: _) =>
            serialVUID = Some(value.longValue);
          case Pair(RemoteAttr, _) =>
            parents = parents ::: List(RemoteInterface.tpe);
            remoteClass = true;
          case _ => ();
        }
      }
      parents = parents.removeDuplicates;

      if (parents.length > 1 ) {
        ifaces = new Array[String](parents.length - 1);
        parents.drop(1).map((s) => javaName(s.symbol)).copyToArray(ifaces, 0);
        ()
      }

      jclass = fjbgContext.JClass(javaFlags(c.symbol),
                                  name,
                                  javaName(parents(0).symbol),
                                  ifaces,
                                  c.cunit.source.toString());

      if (isTopLevelModule(c.symbol) || serialVUID != None) {
        if (isTopLevelModule(c.symbol))
            addModuleInstanceField;
        addStaticInit(jclass);

        if (c.symbol.linkedClass != NoSymbol)
          log("No mirror class for module with linked class: " + c.symbol.fullNameString);
        else
          dumpMirrorClass;
      }

      clasz.fields foreach genField;
      clasz.methods foreach genMethod;

      emitClass(jclass, c.symbol)
    }

    def isTopLevelModule(sym: Symbol): Boolean = {
      sym.isModuleClass && !sym.isImplClass && !sym.hasFlag(Flags.LIFTED) /* && !atPhase(currentRun.erasurePhase)(sym.isNestedClass) */
    }

    def genField(f: IField): Unit  = {
      if (settings.debug.value)
        log("Adding field: " + f.symbol.fullNameString);
      var attributes = 0;

      f.symbol.attributes foreach { a => a match {
        case Pair(TransientAtt, _) => attributes = attributes | JAccessFlags.ACC_TRANSIENT;
        case Pair(VolatileAttr, _) => attributes = attributes | JAccessFlags.ACC_VOLATILE;
        case _ => ();
      }}
      jclass.addNewField(javaFlags(f.symbol) | attributes,
                         javaName(f.symbol),
                         javaType(toTypeKind(f.symbol.tpe)));
    }

    def genMethod(m: IMethod): Unit = {
      if (settings.debug.value)
        log("Generating method " + m.symbol + " flags: " + Flags.flagsToString(m.symbol.flags) +
            " owner: " + m.symbol.owner);
      method = m;
      endPC.clear;
      computeLocalVarsIndex(m);

      var resTpe = javaType(toTypeKind(m.symbol.tpe.resultType));
      if (m.symbol.isClassConstructor)
        resTpe = JType.VOID;

      var flags = javaFlags(m.symbol);
      if (jclass.isInterface())
        flags = flags | JAccessFlags.ACC_ABSTRACT;

      jmethod = jclass.addNewMethod(flags,
                                    javaName(m.symbol),
                                    resTpe,
                                    javaTypes(m.params map (.kind)),
                                    javaNames(m.params map (.sym)));

      if (m.symbol.hasFlag(Flags.BRIDGE))
        jmethod.addAttribute(fjbgContext.JOtherAttribute(jclass, jmethod, "Bridge",
                                                         new Array[Byte](0)));
      if (remoteClass || (m.symbol.attributes contains Pair(RemoteAttr, Nil)))
        if (jmethod.isPublic()) {
          // (see http://java.sun.com/docs/books/vmspec/html/ClassFile.doc.html#3129)
          // Exceptions_attribute {
          // ..
          // u2 number_of_exceptions;
    	  // u2 exception_index_table[number_of_exceptions];
          // }
          val cp = jclass.getConstantPool();
          val reInx = cp.addClass(JAVA_RMI_REMOTEEXCEPTION);
          val contents = java.nio.ByteBuffer.allocate(4); // u2 + u2[1]
          contents.putShort(1.asInstanceOf[Short]);
          contents.putShort(reInx.asInstanceOf[Short]);
          if (settings.debug.value)
            log("adding 'Exceptions_attribute' " + contents + " for remote method " + method);
          jmethod.addAttribute(
            fjbgContext.JOtherAttribute(jclass, jmethod, "Exceptions", contents.array()));
        }

      if (!jmethod.isAbstract()) {
        for (val local <- m.locals; (! m.params.contains(local))) {
          if (settings.debug.value)
            log("add local var: " + local);
          jmethod.addNewLocalVariable(javaType(local.kind), javaName(local.sym));
        }

        jcode = jmethod.getCode().asInstanceOf[JExtendedCode];
        genCode(m);
        genLocalVariableTable;
      }
    }

    def addModuleInstanceField: Unit = {
      import JAccessFlags._;
      jclass.addNewField(ACC_PUBLIC | ACC_FINAL | ACC_STATIC,
                        MODULE_INSTANCE_NAME,
                        jclass.getType());
    }

    def addStaticInit(cls: JClass): Unit = {
      import JAccessFlags._;
      val clinitMethod = cls.addNewMethod(ACC_PUBLIC | ACC_STATIC,
                                          "<clinit>",
                                          JType.VOID,
                                          JType.EMPTY_ARRAY,
                                          new Array[String](0));
      val clinit = clinitMethod.getCode().asInstanceOf[JExtendedCode];
      if (isTopLevelModule(clasz.symbol)) {
        clinit.emitNEW(cls.getName());
        clinit.emitDUP();
        clinit.emitINVOKESPECIAL(cls.getName(),
                                 JMethod.INSTANCE_CONSTRUCTOR_NAME,
                                 JMethodType.ARGLESS_VOID_FUNCTION);
      }

      serialVUID match {
        case Some(value) =>
          val fieldName = "serialVersionUID";
          jclass.addNewField(JAccessFlags.ACC_STATIC | JAccessFlags.ACC_PUBLIC,
                             fieldName,
                             JType.LONG);
          clinit.emitPUSH(value);
          clinit.emitPUTSTATIC(jclass.getName(), fieldName, JType.LONG);
        case None => ();
      }

      clinit.emitRETURN();
    }

    def dumpMirrorClass: Unit = {
      import JAccessFlags._;
      assert(clasz.symbol.isModuleClass);
      if (settings.debug.value)
        log("Dumping mirror class for object: " + clasz);
      val moduleName = javaName(clasz.symbol); // + "$";
      val mirrorName = moduleName.substring(0, moduleName.length() - 1);
      val mirrorClass = fjbgContext.JClass(ACC_SUPER | ACC_PUBLIC | ACC_FINAL,
                                           mirrorName,
                                           "java.lang.Object",
                                           JClass.NO_INTERFACES,
                                           clasz.cunit.source.toString());
      for (val m <- clasz.symbol.tpe.nonPrivateMembers;
           m.owner != definitions.ObjectClass && !m.hasFlag(Flags.PROTECTED) &&
           m.isMethod && !m.isConstructor && !isStaticSymbol(m) )
      {
        val paramJavaTypes = m.tpe.paramTypes map (t => toTypeKind(t));
        val paramNames: Array[String] = new Array[String](paramJavaTypes.length);
        for (val i <- Iterator.range(0, paramJavaTypes.length))
          paramNames(i) = "x_" + i;
        val mirrorMethod = mirrorClass
        .addNewMethod(ACC_PUBLIC | ACC_FINAL | ACC_STATIC,
                      javaName(m),
                      javaType(toTypeKind(m.tpe.resultType)),
                      javaTypes(paramJavaTypes),
                      paramNames);
        val mirrorCode = mirrorMethod.getCode().asInstanceOf[JExtendedCode];
        mirrorCode.emitGETSTATIC(moduleName,
                                 MODULE_INSTANCE_NAME,
                                 new JObjectType(moduleName));
        var i = 0;
        var index = 0;
        var argTypes = mirrorMethod.getArgumentTypes();
        while (i < argTypes.length) {
          mirrorCode.emitLOAD(index, argTypes(i));
          index = index + argTypes(i).getSize();
          i = i + 1;
        }

        mirrorCode.emitINVOKEVIRTUAL(moduleName, mirrorMethod.getName(), mirrorMethod.getType().asInstanceOf[JMethodType]);
        mirrorCode.emitRETURN(mirrorMethod.getReturnType());
      }
      emitClass(mirrorClass, clasz.symbol);
    }

    var linearization: List[BasicBlock] = Nil;

    var isModuleInitialized = false;
    def genCode(m: IMethod): Unit = {
      labels.clear;
      isModuleInitialized = false;

      code = m.code;
      linearization = linearizer.linearize(m);
      makeLabels(linearization);
      genBlocks(linearization);

      if (this.method.exh != Nil)
        genExceptionHandlers;
    }

    var nextBlock: BasicBlock = _;

    def genBlocks(l: List[BasicBlock]): Unit = l match {
      case Nil => ();
      case x :: Nil => nextBlock = null; genBlock(x);
      case x :: y :: ys => nextBlock = y; genBlock(x); genBlocks(y :: ys);
    }


    /** Generate exception handlers for the current method. */
    def genExceptionHandlers: Unit = {

      def ranges(e: ExceptionHandler): List[Pair[Int, Int]] = {
        var covered = e.covered;
        var ranges: List[Pair[Int, Int]] = Nil;
        var start = -1;
        var end = -1;

        linearization foreach ((b) => {
          if (! (covered contains b) ) {
            if (start >= 0) { // we're inside a handler range
              end = labels(b).getAnchor();
              ranges = Pair(start, end) :: ranges;
              start = -1;
            }
          } else {
            if (start >= 0) { // we're inside a handler range
              end = endPC(b);
            } else {
              start = labels(b).getAnchor();
              end   = endPC(b);
            }
            covered = covered remove b.==;
          }
        });

        if (start >= 0) {
          ranges = Pair(start, end) :: ranges;
        }

        if (covered != Nil)
          if (settings.debug.value)
            log("Some covered blocks were not found in method: " + method +
                " covered: " + covered + " not in " + linearization);
        ranges
      }

      this.method.exh foreach ((e) => {
        ranges(e).sort({ (p1, p2) => p1._1 < p2._1 })
        .foreach ((p) => {
          if (settings.debug.value)
            log("Adding exception handler " + e + "at block: " + e.startBlock + " for " + method +
                " from: " + p._1 + " to: " + p._2 + " catching: " + e.cls);
          jcode.addExceptionHandler(p._1, p._2,
                                    labels(e.startBlock).getAnchor(),
                                    if (e.cls == NoSymbol)
                                      null
                                    else javaName(e.cls))
        })
      });
    }

    def genBlock(b: BasicBlock): Unit = {
      labels(b).anchorToNext();

      if (settings.debug.value)
        log("Generating code for block: " + b + " at pc: " + labels(b).getAnchor());
      var lastMappedPC = 0;
      var lastLineNr =0;
      var crtPC = 0;

      b traverse ( instr => {
        if (b.lastInstruction == instr)
          endPC(b) = jcode.getPC();

        instr match {
          case THIS(clasz) =>
            jcode.emitALOAD_0();

          case CONSTANT(const) =>
            const.tag match {
              case UnitTag    => ();
              case BooleanTag => jcode.emitPUSH(const.booleanValue);
              case ByteTag    => jcode.emitPUSH(const.byteValue);
              case ShortTag   => jcode.emitPUSH(const.shortValue);
              case CharTag    => jcode.emitPUSH(const.charValue);
              case IntTag     => jcode.emitPUSH(const.intValue);
              case LongTag    => jcode.emitPUSH(const.longValue);
              case FloatTag   => jcode.emitPUSH(const.floatValue);
              case DoubleTag  => jcode.emitPUSH(const.doubleValue);
              case StringTag  => jcode.emitPUSH(const.stringValue);
              case NullTag    => jcode.emitACONST_NULL();
              case _          => abort("Unknown constant value: " + const);
            }

          case LOAD_ARRAY_ITEM(kind) =>
            jcode.emitALOAD(javaType(kind));

          case LOAD_LOCAL(local, isArg) =>
            if (isArg)
              jcode.emitLOAD(indexOf(local), javaType(local.kind));
            else
              jcode.emitLOAD(indexOf(local), javaType(local.kind));

          case LOAD_FIELD(field, isStatic) =>
            var owner = javaName(field.owner);
//            if (field.owner.hasFlag(Flags.MODULE)) owner = owner + "$";
            if (settings.debug.value)
              log("LOAD_FIELD with owner: " + owner + " flags: " + Flags.flagsToString(field.owner.flags));
            if (isStatic)
              jcode.emitGETSTATIC(owner,
                                  javaName(field),
                                  javaType(field));
            else
              jcode.emitGETFIELD(owner,
                                  javaName(field),
                                  javaType(field));

          case LOAD_MODULE(module) =>
            assert(module.isModule || module.isModuleClass, "Expected module: " + module);
            if (settings.debug.value)
              log("genearting LOAD_MODULE for: " + module + " flags: " +
                  Flags.flagsToString(module.flags));
            jcode.emitGETSTATIC(javaName(module) /* + "$" */ ,
                                MODULE_INSTANCE_NAME,
                                javaType(module));

          case STORE_ARRAY_ITEM(kind) =>
            jcode.emitASTORE(javaType(kind));

          case STORE_LOCAL(local, isArg) =>
            if (isArg)
              jcode.emitSTORE(indexOf(local), javaType(local.kind));
            else
              jcode.emitSTORE(indexOf(local), javaType(local.kind));

          case STORE_FIELD(field, isStatic) =>
            val owner = javaName(field.owner); // + (if (field.owner.hasFlag(Flags.MODULE)) "$" else "");
            if (isStatic)
              jcode.emitPUTSTATIC(owner,
                                  javaName(field),
                                  javaType(field));
            else
              jcode.emitPUTFIELD(owner,
                                  javaName(field),
                                  javaType(field));

          case CALL_PRIMITIVE(primitive) =>
            genPrimitive(primitive, instr.pos);

          // TODO: reference the type of the receiver instead of the
          // method owner.
          case CALL_METHOD(method, style) =>
            val owner = javaName(method.owner); // + (if (method.owner.isModuleClass) "$" else "");

            style match {
              case Dynamic =>
                if (method.owner.hasFlag(Flags.INTERFACE))
                  jcode.emitINVOKEINTERFACE(owner,
                                            javaName(method),
                                            javaType(method).asInstanceOf[JMethodType])
                else
                  jcode.emitINVOKEVIRTUAL(owner,
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);

              case Static(instance) =>
                if (instance) {
                  jcode.emitINVOKESPECIAL(owner,
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);
                } else
                  jcode.emitINVOKESTATIC(owner,
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);

              case SuperCall(_) =>
                  jcode.emitINVOKESPECIAL(owner,
                                          javaName(method),
                                          javaType(method).asInstanceOf[JMethodType]);
                  // we initialize the MODULE$ field immediately after the super ctor
                  if (isTopLevelModule(clasz.symbol) && !isModuleInitialized &&
                      jmethod.getName() == JMethod.INSTANCE_CONSTRUCTOR_NAME &&
                      javaName(method) == JMethod.INSTANCE_CONSTRUCTOR_NAME) {
                        isModuleInitialized = true;
                        jcode.emitALOAD_0();
                        jcode.emitPUTSTATIC(jclass.getName(),
                                            MODULE_INSTANCE_NAME,
                                            jclass.getType());
                      }


            }

          case NEW(REFERENCE(cls)) =>
            val className = javaName(cls);
            jcode.emitNEW(className);

          case CREATE_ARRAY(elem) => elem match {
            case REFERENCE(_) | ARRAY(_) =>
              jcode.emitANEWARRAY(javaType(elem).asInstanceOf[JReferenceType]);
            case _ =>
              jcode.emitNEWARRAY(javaType(elem));
          }

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
            if (settings.debug.value)
              log("Emitting SWITHCH:\ntags: " + tags + "\nbranches: " + branches);
            jcode.emitSWITCH(tagArray,
                             (branches map labels dropRight 1).copyToArray(branchArray, 0),
                             labels(branches.last),
                             MIN_SWITCH_DENSITY);

          case JUMP(where) =>
            if (nextBlock != where)
              jcode.emitGOTO_maybe_W(labels(where), false); // default to short jumps

          case CJUMP(success, failure, cond, kind) =>
            kind match {
              case BOOL | BYTE | CHAR | SHORT | INT =>
                if (nextBlock == success) {
                  jcode.emitIF_ICMP(conds(negate(cond)), labels(failure));
                  // .. and fall through to success label
                } else {
                  jcode.emitIF_ICMP(conds(cond), labels(success));
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }

              case REFERENCE(_) | ARRAY(_) =>
                if (nextBlock == success) {
                  jcode.emitIF_ACMP(conds(negate(cond)), labels(failure));
                  // .. and fall through to success label
                } else {
                  jcode.emitIF_ACMP(conds(cond), labels(success));
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }

              case _ =>
                kind match {
                  case LONG   => jcode.emitLCMP();
                  case FLOAT  => jcode.emitFCMPG();
                  case DOUBLE => jcode.emitDCMPG();
                }
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure));
                  // .. and fall through to success label
                } else {
                  jcode.emitIF(conds(cond), labels(success));
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }
            }

          case CZJUMP(success, failure, cond, kind) =>
            kind match {
              case BOOL | BYTE | CHAR | SHORT | INT =>
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure));
                } else {
                  jcode.emitIF(conds(cond), labels(success));
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }

              case REFERENCE(_) | ARRAY(_) =>
                if (nextBlock == success) {
                  jcode.emitIFNONNULL(labels(failure));
                } else {
                  jcode.emitIFNULL(labels(success));
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }

              case _ =>
                kind match {
                  case LONG   => jcode.emitLCONST_0(); jcode.emitLCMP();
                  case FLOAT  => jcode.emitFCONST_0(); jcode.emitFCMPL();
                  case DOUBLE => jcode.emitDCONST_0(); jcode.emitDCMPL();
                }
                if (nextBlock == success) {
                  jcode.emitIF(conds(negate(cond)), labels(failure));
                } else {
                  jcode.emitIF(conds(cond), labels(success));
                  if (nextBlock != failure)
                    jcode.emitGOTO_maybe_W(labels(failure), false);
                }
            }

          case RETURN(kind) =>
            jcode.emitRETURN(javaType(kind));

          case THROW() =>
            jcode.emitATHROW();

          case DROP(kind) =>
            kind match {
              case LONG | DOUBLE => jcode.emitPOP2();
              case _ => jcode.emitPOP();
            }

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

        crtPC = jcode.getPC();
        val crtLine = try { clasz.cunit.position(instr.pos).line; } catch {
            case _: Error => lastLineNr;
        }
	//System.err.println("CRTLINE: " + instr.pos + " " +
	//	   /* (if (instr.pos < clasz.cunit.source.content.length) clasz.cunit.source.content(instr.pos) else '*') + */ " " + crtLine);

        if (crtPC > lastMappedPC) {
          jcode.completeLineNumber(lastMappedPC, crtPC, crtLine);
          lastMappedPC = crtPC;
          lastLineNr   = crtLine;
        }

      });
    }


    def genPrimitive(primitive: Primitive, pos: Int): Unit = {
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

        case Logical(op, kind) => Pair(op, kind) match {
          case Pair(AND, LONG) =>
            jcode.emitLAND();
          case Pair(AND, INT) =>
            jcode.emitIAND();
          case Pair(AND, _) =>
            jcode.emitIAND();
            if (kind != BOOL)
              jcode.emitT2T(javaType(INT), javaType(kind));

          case Pair(OR, LONG) =>
            jcode.emitLOR();
          case Pair(OR, INT) =>
            jcode.emitIOR();
          case Pair(OR, _) =>
            jcode.emitIOR();
            if (kind != BOOL)
              jcode.emitT2T(javaType(INT), javaType(kind));

          case Pair(XOR, LONG) =>
            jcode.emitLXOR();
          case Pair(XOR, INT) =>
            jcode.emitIXOR();
          case Pair(XOR, _) =>
            jcode.emitIXOR();
            if (kind != BOOL)
              jcode.emitT2T(javaType(INT), javaType(kind));
        }

        case Shift(op, kind) => Pair(op, kind) match {
          case Pair(LSL, LONG) =>
            jcode.emitLSHL();
          case Pair(LSL, INT) =>
            jcode.emitISHL();
          case Pair(LSL, _) =>
            jcode.emitISHL();
            jcode.emitT2T(javaType(INT), javaType(kind));

          case Pair(ASR, LONG) =>
            jcode.emitLSHR();
          case Pair(ASR, INT) =>
            jcode.emitISHR();
          case Pair(ASR, _) =>
            jcode.emitISHR();
            jcode.emitT2T(javaType(INT), javaType(kind));

          case Pair(LSR, LONG) =>
            jcode.emitLUSHR();
          case Pair(LSR, INT) =>
            jcode.emitIUSHR();
          case Pair(LSR, _) =>
            jcode.emitIUSHR();
            jcode.emitT2T(javaType(INT), javaType(kind));
        }

        case Conversion(src, dst) =>
          if (settings.debug.value)
            log("Converting from: " + src + " to: " + dst);
          if (dst == BOOL) {
            Console.println("Illegal conversion at: " + clasz +
                            " at: " + method.sourceFile + ":" + Position.line(clasz.cunit.source, pos));
          } else
            jcode.emitT2T(javaType(src), javaType(dst));

        case ArrayLength(_) =>
          jcode.emitARRAYLENGTH();

        case StartConcat =>
          jcode.emitNEW(JAVA_LANG_STRINGBUFFER);
          jcode.emitDUP();
          jcode.emitINVOKESPECIAL(JAVA_LANG_STRINGBUFFER,
                                  JMethod.INSTANCE_CONSTRUCTOR_NAME,
                                  JMethodType.ARGLESS_VOID_FUNCTION);

        case StringConcat(el) =>
          val jtype = el match {
            case REFERENCE(_) | ARRAY(_)=> JObjectType.JAVA_LANG_OBJECT;
            case _ => javaType(el);
          }
          jcode.emitINVOKEVIRTUAL(JAVA_LANG_STRINGBUFFER,
                                  "append",
                                  new JMethodType(stringBufferType,
                                                  Predef.Array(jtype)));
        case EndConcat =>
          jcode.emitINVOKEVIRTUAL(JAVA_LANG_STRINGBUFFER,
                                  "toString",
                                  toStringType);

        case _ => abort("Unimplemented primitive " + primitive);
      }
    }

    val endPC: HashMap[BasicBlock, Int] = new HashMap();
    val labels: HashMap[BasicBlock, JCode$Label] = new HashMap();
    val conds: HashMap[TestOp, Int] = new HashMap();

    conds += EQ -> JExtendedCode.COND_EQ;
    conds += NE -> JExtendedCode.COND_NE;
    conds += LT -> JExtendedCode.COND_LT;
    conds += GT -> JExtendedCode.COND_GT;
    conds += LE -> JExtendedCode.COND_LE;
    conds += GE -> JExtendedCode.COND_GE;

    val negate: HashMap[TestOp, TestOp] = new HashMap();

    negate += EQ -> NE;
    negate += NE -> EQ;
    negate += LT -> GE;
    negate += GT -> LE;
    negate += LE -> GT;
    negate += GE -> LT;

    def makeLabels(bs: List[BasicBlock]) = {
      //labels.clear;
      if (settings.debug.value)
        log("Making labels for: " + method);
      bs foreach (bb => labels += bb -> jcode.newLabel() );
    }


    ////////////////////// local vars ///////////////////////

    def sizeOf(sym: Symbol): Int = sizeOf(toTypeKind(sym.tpe));


    def sizeOf(k: TypeKind): Int = k match {
      case DOUBLE | LONG => 2;
      case _ => 1;
    }

    def indexOf(m: IMethod, sym: Symbol): Int = {
      val Some(local) = m.lookupLocal(sym);
      assert (local.index >= 0,
              "Invalid index for: " + local);
      local.index
    }

    def indexOf(local: Local): Int = {
      assert (local.index >= 0,
              "Invalid index for: " + local);
      local.index
    }

    /**
     * Compute the indexes of each local variable of the given
     * method.
     */
    def computeLocalVarsIndex(m: IMethod): Unit = {
      var idx = 1;
      if (isStaticSymbol(m.symbol))
        idx = 0;

      for (val l <- m.locals) {
        if (settings.debug.value)
          log("Index value for " + l + ": " + idx);
        l.index = idx;
        idx = idx + sizeOf(l.kind);
      }
    }

    ////////////////////// Utilities ////////////////////////

    /** Return the a name of this symbol that can be used on the Java
     * platform. It removes spaces from names.
     *
     * Special handling: scala.All and scala.AllRef are 'erased' to
     * scala.All$ and scala.AllRef$. This is needed because they are
     * not real classes, and they mean 'abrupt termination upon evaluation
     * of that expression' or 'null' respectively. This handling is
     * done already in GenICode, but here we need to remove references
     * from method signatures to these types, because such classes can
     * not exist in the classpath: the type checker will be very confused.
     */
    def javaName(sym: Symbol): String = {
      val suffix = if (sym.hasFlag(Flags.MODULE) && !sym.isMethod &&
                        !sym.isImplClass &&
                        !sym.hasFlag(Flags.JAVA)) "$" else "";

      if (sym == definitions.AllClass)
        return "scala.All$"
      else if (sym == definitions.AllRefClass)
        return "scala.AllRef$";

      (if (sym.isClass || (sym.isModule && !sym.isMethod))
        sym.fullNameString('/')
      else
        {
          if ( sym.hasFlag(Flags.ACCESSOR) && sym.attributes.exists(a => a match{
            case Pair(BeanPropertyAttr, _) => true
            case _ => false
          }))
            {
              if (sym.isSetter)
                "set" + nme.setterToGetter(sym.simpleName).toString()
              else "get" + sym.simpleName.toString()
            }
              else {
                sym.simpleName.toString()
          }.trim()
        }
      ) + suffix;
    }

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
      jf = jf | (if ((sym hasFlag Flags.FINAL) && !sym.enclClass.hasFlag(Flags.INTERFACE)) ACC_FINAL else 0);
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

    def getFile(cls: JClass, suffix: String): String = {
      val path = cls.getName().replace('.', File.separatorChar);
      settings.outdir.value + File.separatorChar + path + suffix
    }

    private def genLocalVariableTable: Unit = {
        val vars: Array[JLocalVariable] = jmethod.getLocalVariables();

        if (!settings.debuginfo.value || vars.length == 0)
            return;

        val pool = jclass.getConstantPool();
        val pc = jcode.getPC();
        var anonCounter = 0;

        val lvTab = java.nio.ByteBuffer.allocate(2 + 10 * vars.length);
        lvTab.putShort(vars.length.asInstanceOf[Short]);
        for (val lv <- vars) {
            val name = if (lv.getName() == null) {
              anonCounter = anonCounter + 1;
              "<anon" + anonCounter + ">"
            } else lv.getName();

            lvTab.putShort(0.asInstanceOf[Short]);
            lvTab.putShort(pc.asInstanceOf[Short]);
            lvTab.putShort(pool.addUtf8(name).asInstanceOf[Short]);
            lvTab.putShort(pool.addUtf8(lv.getType().getSignature()).asInstanceOf[Short]);
            lvTab.putShort(lv.getIndex().asInstanceOf[Short]);
        }
        val attr =
            fjbgContext.JOtherAttribute(jclass,
                                        jmethod,
                                        "LocalVariableTable",
                                        lvTab.array());
        jcode.addAttribute(attr);
    }
  }
}
