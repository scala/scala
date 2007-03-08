/* NSC -- new scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 */

// $Id$

package scala.tools.nsc.backend.msil

import java.io.File
import java.nio.{ByteBuffer, ByteOrder}

import scala.collection.mutable.{Map, HashMap, HashSet, Stack}
import scala.tools.nsc.symtab._
import scala.tools.nsc.util.Position
import compat.StringBuilder

import ch.epfl.lamp.compiler.msil.{Type => MsilType, _}
import ch.epfl.lamp.compiler.msil.emit._

/**
 */
abstract class GenMSIL extends SubComponent {
  import global._
  import loaders.clrTypes
  import clrTypes.{types, constructors, methods, fields}
  import icodes._
  import icodes.opcodes._

  /** Create a new phase */
  override def newPhase(p: Phase) = new MsilPhase(p)

  val phaseName = "msil"
  /** MSIL code generation phase
   */
  class MsilPhase(prev: Phase) extends GlobalPhase(prev) {
    def name = phaseName
    override def newFlags = phaseNewFlags

    override def erasedTypes = true

    override def run: Unit = {
      if (settings.debug.value) inform("[running phase " + name + " on icode]")

      val codeGenerator = new BytecodeGenerator

      //classes is ICodes.classes, a HashMap[Symbol, IClass]
      classes.values foreach codeGenerator.findEntryPoint

      codeGenerator.initAssembly

      classes.values foreach codeGenerator.createTypeBuilder
      classes.values foreach codeGenerator.createClassMembers


      try {
        classes.values foreach codeGenerator.genClass
      } finally {
        codeGenerator.writeAssembly
      }
    }

    override def apply(unit: CompilationUnit): Unit =
      abort("MSIL works on icode classes, not on compilation units!")
  }

  /**
   * MSIL bytecode generator.
   *
   */
  class BytecodeGenerator {

    val MODULE_INSTANCE_NAME = "MODULE$"

    import clrTypes.{VOID => MVOID, BOOLEAN => MBOOL, UBYTE => MBYTE, SHORT => MSHORT,
                   CHAR => MCHAR, INT => MINT, LONG => MLONG, FLOAT => MFLOAT,
                   DOUBLE => MDOUBLE, OBJECT => MOBJECT, STRING => MSTRING,
                   STRING_ARRAY => MSTRING_ARRAY, SCALA_SYMTAB_ATTR => SYMTAB_ATTRIBUTE,
                   SYMTAB_CONSTR => SYMTAB_ATTRIBUTE_CONSTRUCTOR,
                   SYMTAB_DEFAULT_CONSTR => SYMTAB_ATTRIBUTE_EMPTY_CONSTRUCTOR}

    val EXCEPTION = clrTypes.getType("System.Exception")
    val MBYTE_ARRAY = clrTypes.mkArrayType(MBYTE)

    val ICLONEABLE = clrTypes.getType("System.ICloneable")
    val MEMBERWISE_CLONE = MOBJECT.GetMethod("MemberwiseClone", MsilType.EmptyTypes)

    val MMONITOR       = clrTypes.getType("System.Threading.Monitor")
    val MMONITOR_ENTER = MMONITOR.GetMethod("Enter", Array(MOBJECT))
    val MMONITOR_EXIT  = MMONITOR.GetMethod("Exit", Array(MOBJECT))

    val MSTRING_BUILDER = clrTypes.getType("System.Text.StringBuilder")
    val MSTRING_BUILDER_CONSTR = MSTRING_BUILDER.GetConstructor(MsilType.EmptyTypes)
    val MSTRING_BUILDER_TOSTRING = MSTRING_BUILDER.GetMethod("ToString",
							     MsilType.EmptyTypes)

    val TYPE_FROM_HANDLE =
      clrTypes.getType("System.Type").GetMethod("GetTypeFromHandle", Array(clrTypes.getType("System.RuntimeTypeHandle")))

    val INT_PTR = clrTypes.getType("System.IntPtr")

    val JOBJECT = definitions.ObjectClass
    val JSTRING = definitions.StringClass

    var JSTRING_SUBSTRING_INT_INT: Symbol = _

    val SystemConvert = clrTypes.getType("System.Convert")

    val objParam = Array(MOBJECT)

//     val toBool:   MethodInfo = SystemConvert.GetMethod("ToBoolean", objParam)
    val toByte:   MethodInfo = SystemConvert.GetMethod("ToByte", objParam)
    val toShort:  MethodInfo = SystemConvert.GetMethod("ToInt16", objParam)
    val toChar:   MethodInfo = SystemConvert.GetMethod("ToChar", objParam)
    val toInt:    MethodInfo = SystemConvert.GetMethod("ToInt32", objParam)
    val toLong:   MethodInfo = SystemConvert.GetMethod("ToInt64", objParam)
    val toFloat:  MethodInfo = SystemConvert.GetMethod("ToSingle", objParam)
    val toDouble: MethodInfo = SystemConvert.GetMethod("ToDouble", objParam)

    //val boxedUnit: FieldInfo = msilType(definitions.BoxedUnitModule.info).GetField("UNIT")
    val boxedUnit: FieldInfo = fields(definitions.BoxedUnit_UNIT)

    // Scala attributes
    // symtab.Definitions -> object (singleton..)
    val SerializableAttr = definitions.SerializableAttr.tpe
    val CloneableAttr    = definitions.getClass("scala.cloneable").tpe
    val TransientAtt     = definitions.getClass("scala.transient").tpe
    // remoting: the architectures are too different, no mapping (no portable code
    // possible)

    // java instance methods that are mapped to static methods in .net
    // these will need to be called with OpCodes.Call (not Callvirt)
    val dynToStatMapped: HashSet[Symbol] = new HashSet()

    initMappings()
    // ********************************************************************
    // Create the mappings
    private def initMappings(): Unit = {
      mapType(definitions.AnyClass, MOBJECT)
      mapType(definitions.AnyRefClass, MOBJECT)
      //mapType(definitions.AllRefClass, clrTypes.getType("scala.AllRef$"))
      //mapType(definitions.AllClass, clrTypes.getType("scala.All$"))
      // FIXME: for some reason the upper two lines map to null
      mapType(definitions.AllRefClass, EXCEPTION)
      mapType(definitions.AllClass, EXCEPTION)

      val jEmpty = new Array[Type](0)
      val jString1 = Array(JSTRING.tpe)
      val jInt1 = Array(definitions.IntClass.tpe)
      val jInt2 = Array(definitions.IntClass.tpe, definitions.IntClass.tpe)
      val jLong1 = Array(definitions.LongClass.tpe)
      val jStringInt = Array(JSTRING.tpe, definitions.IntClass.tpe)
      val jChar2 = Array(definitions.CharClass.tpe, definitions.CharClass.tpe)

      val mObject1 = Array(MOBJECT)
      val mString1 = Array(MSTRING)
      val mString2 = Array(MSTRING, MSTRING)
      val mChar1 = Array(MCHAR)
      val mCharInt = Array(MCHAR, MINT)

      JSTRING_SUBSTRING_INT_INT = lookupMethod(JSTRING, "substring", jInt2)

      mapMethod(JOBJECT, "clone", MOBJECT, "MemberwiseClone")
      mapMethod(JOBJECT, nme.equals_, MOBJECT, "Equals")
      mapMethod(JOBJECT, nme.hashCode_, MOBJECT, "GetHashCode")
      mapMethod(JOBJECT, nme.toString_, MOBJECT, "ToString")
      mapMethod(JOBJECT, nme.finalize_, MOBJECT, "Finalize")
      mapMethod(JOBJECT, nme.wait_, jEmpty, MMONITOR, "Wait", mObject1)
      mapMethod(JOBJECT, nme.wait_, jLong1, MMONITOR, "Wait", Array(MOBJECT, MINT))
      mapMethod(JOBJECT, nme.notify_, jEmpty, MMONITOR, "Pulse", mObject1)
      mapMethod(JOBJECT, nme.notifyAll_, jEmpty, MMONITOR, "PulseAll", mObject1)

      mapMethod(JSTRING, "compareTo",MSTRING, "CompareTo")
      mapMethod(JSTRING, "length", MSTRING, "get_Length")
      mapMethod(JSTRING, "charAt", MSTRING, "get_Chars")

      mapMethod(JSTRING, "concat", jString1, MSTRING, "Concat", mString2)
      mapMethod(JSTRING, "indexOf", jInt1, MSTRING, "IndexOf", mChar1)
      mapMethod(JSTRING, "indexOf", jInt2, MSTRING, "IndexOf", mCharInt)

      mapMethod(JSTRING, "indexOf", jString1, MSTRING, "IndexOf")
      mapMethod(JSTRING, "indexOf", jStringInt, MSTRING, "IndexOf")
      mapMethod(JSTRING, "lastIndexOf", jInt1, MSTRING, "LastIndexOf", mChar1)
      mapMethod(JSTRING, "lastIndexOf", jInt2, MSTRING, "LastIndexOf", mCharInt)
      mapMethod(JSTRING, "lastIndexOf", jString1, MSTRING, "LastIndexOf")
      mapMethod(JSTRING, "lastIndexOf", jStringInt, MSTRING, "LastIndexOf")

      mapMethod(JSTRING, "toLowerCase", jEmpty, MSTRING, "ToLower")
      mapMethod(JSTRING, "toUpperCase", jEmpty, MSTRING, "ToUpper")
      mapMethod(JSTRING, "startsWith", jString1, MSTRING, "StartsWith")
      mapMethod(JSTRING, "endsWith", jString1, MSTRING, "EndsWith")
      mapMethod(JSTRING, "substring", jInt1, MSTRING, "Substring")
      mapMethod(JSTRING, "substring", jInt2, MSTRING, "Substring")
      mapMethod(JSTRING, "trim", jEmpty, MSTRING, "Trim")
      mapMethod(JSTRING, "intern", jEmpty, MSTRING, "Intern", mString1)
      mapMethod(JSTRING, "replace", jChar2, MSTRING, "Replace")
      mapMethod(JSTRING, "toCharArray", MSTRING, "ToCharArray")

      mapType(definitions.BooleanClass, MBOOL)
      mapType(definitions.ByteClass, MBYTE)
      mapType(definitions.ShortClass, MSHORT)
      mapType(definitions.CharClass, MCHAR)
      mapType(definitions.IntClass, MINT)
      mapType(definitions.LongClass, MLONG)
      mapType(definitions.FloatClass, MFLOAT)
      mapType(definitions.DoubleClass, MDOUBLE)

    }

    var clasz: IClass = _
    var method: IMethod = _
    var code: Code = _

    var massembly: AssemblyBuilder = _
    var mmodule: ModuleBuilder = _
    var mcode: ILGenerator = _

    var assemName : String = _
    var firstSourceName = ""
    var outDir : File = _

    def initAssembly(): Unit = {

      assemName = settings.assemname.value
      if (assemName == "") {

	if (entryPoint != null) {
	  assemName = msilName(entryPoint.enclClass)
	  // remove the $ at the end (from module-name)
	  assemName = assemName.substring(0, assemName.length() - 1)
	} else {
	  // assuming filename of first source file
	  assert(firstSourceName.endsWith(".scala"), "Source file doesn't end with .scala")
	  assemName = firstSourceName.substring(0, firstSourceName.length() - 6)
	}
      } else {
	if (assemName.endsWith(".msil"))
	  assemName = assemName.substring(0, assemName.length()-5)
	if (assemName.endsWith(".il"))
	  assemName = assemName.substring(0, assemName.length()-3)
	val f: File = new File(assemName)
	outDir = f.getParentFile()
	assemName = f.getName()
      }
      if (outDir == null)
	outDir = new File(".")


      val assemblyName = new AssemblyName()
      assemblyName.Name = assemName
      massembly = AssemblyBuilder.DefineDynamicAssembly(assemblyName)

      val moduleName = assemName + (if (entryPoint == null) ".dll" else ".exe")
      // filename here: .dll or .exe (in both parameters), second: give absolute-path
      mmodule = massembly.DefineDynamicModule(moduleName,
					      new File(outDir, moduleName).getAbsolutePath())
      assert (mmodule != null)
      initMappings()

    }


    /**
     * Form of the custom Attribute parameter (Ecma-335.pdf)
     *      - p. 163 for CustomAttrib Form,
     *      - p. 164 for FixedArg Form (Array and Element) (if array or not is known!)
     *  !! least significant *byte* first if values longer than one byte !!
     *
     * 1: Prolog (unsigned int16, value 0x0001) -> symtab[0] = 0x01, symtab[1] = 0x00
     * 2: FixedArgs (directly the data, get number and types from related constructor)
     *  2.1: length of the array (unsigned int32, take care on order of the 4 bytes)
     *  2.2: the byte array data
     * 3: NumNamed (unsigned int16, number of named fields and properties, 0x0000)
     *
     **/
    def addSymtabAttribute(sym: Symbol, tBuilder: TypeBuilder): Unit = currentRun.symData.get(sym) match {
      case Some(pickle) =>
	val symtab: Array[Byte] = new Array[Byte](pickle.writeIndex + 8)
	symtab(0) = 1.toByte
	var size:Int = pickle.writeIndex
	for(val i <- Iterator.range(2, 6)) {
	  symtab(i) = (size & 0xff).toByte
	  size = size >> 8
	}

	System.arraycopy(pickle.bytes, 0, symtab, 6, pickle.writeIndex)

	tBuilder.SetCustomAttribute(SYMTAB_ATTRIBUTE_CONSTRUCTOR, symtab)

        currentRun.symData -= sym
	currentRun.symData -= sym.linkedSym
	//log("Generated ScalaSig Attr for " + sym)//debug
      case _ =>
        log("Could not find pickle information for " + sym)
    }

    def addAttributes(member: ICustomAttributeSetter, attributes: List[AnnotationInfo[Constant]]): Unit = {
      return // FIXME

      if (settings.debug.value)
	log("creating attributes: " + attributes + " for member : " + member)
      for(val AnnotationInfo(typ, consts, nvPairs) <- attributes /* !typ.symbol.hasFlag(Flags.JAVA) */ ) {
//	assert(consts.length <= 1,
//	       "too many constant arguments for attribute; "+consts.toString())

	// Problem / TODO having the symbol of the attribute type would be nicer
	// (i hope that type.symbol is the same as the one in types2create)
	// AND: this will crash if the attribute Type is already compiled (-> not a typeBuilder)
	// when this is solved, types2create will be the same as icodes.classes, thus superfluous
	val attrType: TypeBuilder = getType(typ.symbol).asInstanceOf[TypeBuilder]
//	val attrType: MsilType = getType(typ.symbol)

	// Problem / TODO: i have no idea which constructor is used. This
	// information should be available in AnnotationInfo.
	attrType.CreateType() // else, GetConstructors can't be used
	val constr: ConstructorInfo = attrType.GetConstructors()(0)
	// prevent a second call of CreateType, only needed because there's no
	// otehr way than GetConstructors()(0) to get the constructor, if there's
	// no constructor symbol available.

	val args: Array[Byte] = getAttributeArgs(consts, nvPairs)
	member.SetCustomAttribute(constr, args)
      }
    }

    def getAttributeArgs(consts: List[Constant], nvPairs: List[(Name, Constant)]): Array[Byte] = {
      val buf = ByteBuffer.allocate(2048) // FIXME: this may be not enough!
      buf.order(ByteOrder.LITTLE_ENDIAN)
      buf.putShort(1.toShort) // signature

      def emitSerString(str: String) = {
	  // this is wrong, it has to be the length of the UTF-8 byte array, which
	  // may be longer (see clr-book on page 302)
//	  val length: Int = str.length
	    val strBytes: Array[Byte] = try {
	      str.getBytes("UTF-8")
	    } catch {
	      case _: Error => abort("could not get byte-array for string: " + str)
	    }
	    val length: Int = strBytes.length //this length is stored big-endian
	    if (length < 128)
	      buf.put(length.toByte)
	    else if (length < (1<<14)) {
	      buf.put(((length >> 8) | 0x80).toByte) // the bits 14 and 15 of length are '0'
	      buf.put((length | 0xff).toByte)
	    } else if (length < (1 << 29)) {
              buf.put(((length >> 24) | 0xc0).toByte)
              buf.put(((length >> 16) & 0xff).toByte)
              buf.put(((length >>  8) & 0xff).toByte)
              buf.put(((length      ) & 0xff).toByte)
	    } else
	      abort("string too long for attribute parameter: " + length)
	    buf.put(strBytes)
      }

      def emitConst(const: Constant): Unit = const.tag match {
	case BooleanTag => buf.put((if (const.booleanValue) 1 else 0).toByte)
	case ByteTag => buf.put(const.byteValue)
	case ShortTag => buf.putShort(const.shortValue)
	case CharTag => buf.putChar(const.charValue)
	case IntTag => buf.putInt(const.intValue)
	case LongTag => buf.putLong(const.longValue)
	case FloatTag => buf.putFloat(const.floatValue)
	case DoubleTag => buf.putDouble(const.doubleValue)
	case StringTag =>
	  val str: String = const.stringValue
	  if (str == null) {
	    buf.put(0xff.toByte)
	  } else {
	    emitSerString(str)
	  }
	case ArrayTag =>
	  val arr: Array[Constant] = const.arrayValue
	  if (arr == null) {
	    buf.putInt(0xffffffff)
	  } else {
	    buf.putInt(arr.length)
	    arr.foreach(emitConst)
	  }

	// TODO: other Tags: NoTag, UnitTag, ClassTag, EnumTag, ArrayTag ???

	case _ => abort("could not handle attribute argument: " + const)
      }

      consts foreach emitConst
      buf.putShort(nvPairs.length.toShort)
      def emitNamedArg(nvPair: (Name, Constant)): Unit = {
	// the named argument is a property of the attribute (it can't be a field, since
	//  all fields in scala are private)
	buf.put(0x54.toByte)

	def emitType(c: Constant) = c.tag match { // type of the constant, Ecma-335.pdf, page 151
	  case BooleanTag => buf.put(0x02.toByte)
	  case ByteTag =>    buf.put(0x05.toByte)
	  case ShortTag =>   buf.put(0x06.toByte)
	  case CharTag =>    buf.put(0x07.toByte)
	  case IntTag =>     buf.put(0x08.toByte)
	  case LongTag =>    buf.put(0x0a.toByte)
	  case FloatTag =>   buf.put(0x0c.toByte)
	  case DoubleTag =>  buf.put(0x0d.toByte)
	  case StringTag =>  buf.put(0x0e.toByte)

	  // TODO: other Tags: NoTag, UnitTag, ClassTag, EnumTag ???

	  // ArrayTag falls in here
	  case _ => abort("could not handle attribute argument: " + c)
	}

	val cnst: Constant = nvPair._2
	if (cnst.tag == ArrayTag) {
	  buf.put(0x1d.toByte)
	  emitType(cnst.arrayValue(0)) // FIXME: will crash if array length = 0
	} else if (cnst.tag == EnumTag) {
	  buf.put(0x55.toByte)
	  // TODO: put a SerString (don't know what exactly, names of the enums somehow..)
      	} else {
	  buf.put(0x51.toByte)
	  emitType(cnst)
	}

	emitSerString(nvPair._1.toString)
	emitConst(nvPair._2)
      }

      val length = buf.position()
      buf.array().subArray(0, length)
    }

    def writeAssembly(): Unit = {
      if (entryPoint != null) {
	assert(entryPoint.enclClass.isModuleClass, "main-method not defined in a module")
	val mainMethod = methods(entryPoint)
	val stringArrayTypes: Array[MsilType] = Array(MSTRING_ARRAY)
	val globalMain = mmodule.DefineGlobalMethod(
	  "Main", MethodAttributes.Public | MethodAttributes.Static,
	  MVOID, stringArrayTypes)
	globalMain.DefineParameter(0, ParameterAttributes.None, "args")
	massembly.SetEntryPoint(globalMain)
	val code = globalMain.GetILGenerator()
	val moduleField = getModuleInstanceField(entryPoint.enclClass)
	code.Emit(OpCodes.Ldsfld, moduleField)
	code.Emit(OpCodes.Ldarg_0)
	code.Emit(OpCodes.Callvirt, mainMethod)
	code.Emit(OpCodes.Ret)
      }
      createTypes()
      val filename = new File(outDir, assemName + ".msil").getPath()
      if (settings.debug.value)
	log("output file name: " + filename)
      try {
	massembly.Save(filename)
      } catch {
	case _: Error => abort("Could not save file " + filename)
      }
    }

    private def createTypes(): Unit =
      for (val sym <- classes.keys) {
	if (settings.debug.value)
	  log("Calling CreatType for " +  sym + ", " + types(sym))
	types(sym).asInstanceOf[TypeBuilder].CreateType()
      }

    private[GenMSIL] def genClass(iclass: IClass): Unit = {
      val sym = iclass.symbol
      if (settings.debug.value)
        log("Generating class " + sym + " flags: " + Flags.flagsToString(sym.flags))
      clasz = iclass

      val tBuilder = getType(sym).asInstanceOf[TypeBuilder]
      if (isCloneable(sym)){
	// FIXME: why there's no nme.clone_ ?
	// "Clone": if the code is non-portable, "Clone" is defined, not "clone"
        // TODO: improve condition (should override AnyRef.clone)
	if (iclass.methods.forall(m => {
	  !((m.symbol.name.toString() != "clone" || m.symbol.name.toString() != "Clone") &&
	    m.symbol.tpe.paramTypes.length != 0)
	})) {
	  if (settings.debug.value)
	    log("auto-generating cloneable method for " + sym)
	  val attrs: Short = (MethodAttributes.Public | MethodAttributes.Virtual |
			      MethodAttributes.HideBySig)
	  val cloneMethod = tBuilder.DefineMethod("Clone", attrs, MOBJECT,
						  MsilType.EmptyTypes)
	  val clCode = cloneMethod.GetILGenerator()
	  clCode.Emit(OpCodes.Ldarg_0)
	  clCode.Emit(OpCodes.Call, MEMBERWISE_CLONE)
	  clCode.Emit(OpCodes.Ret)
	}
      }

      tBuilder.setPosition(iclass.cunit.position(sym.pos).line, iclass.cunit.source.file.name)

      if (isTopLevelModule(sym)) {
	if (settings.debug.value)
	  log("TopLevelModule: " + sym)
	if (sym.linkedClassOfModule == NoSymbol) {
	  if (settings.debug.value)
	    log(" no linked class: " + sym)
	  dumpMirrorClass(sym)
	} else if (!currentRun.compiles(sym.linkedClassOfModule)) {
	  if (settings.debug.value)
	    log(" not compiling linked class: " + sym)
	  dumpMirrorClass(sym)
	}
      }

      // the pickling info is not written to the module class, but to it's
      // linked class (the mirror class eventually dumped)
      if (!(tBuilder.Name.endsWith("$") && sym.isModuleClass)){
	// think the if inside could be removed, because in this case, addSymtabAttribute is
	// called in the dumpMirrorClass method
	addSymtabAttribute(if (isTopLevelModule(sym)) sym.sourceModule else sym, tBuilder)

	// TODO: remove; check the above think:
	assert(!isTopLevelModule(sym), "can't remove the 'if'")
      }

      addAttributes(tBuilder, sym.attributes)

      if (iclass.symbol != definitions.ArrayClass)
        iclass.methods foreach genMethod

    } //genClass


    private def genMethod(m: IMethod): Unit = {
      if (settings.debug.value)
	log("Generating method " + m.symbol + " flags: " + Flags.flagsToString(m.symbol.flags) +
	    " owner: " + m.symbol.owner)
      method = m
      localBuilders.clear
      computeLocalVarsIndex(m)

      if (m.symbol.isClassConstructor){
	mcode = constructors(m.symbol).asInstanceOf[ConstructorBuilder].GetILGenerator()
      } else {
	val mBuilder = methods(m.symbol).asInstanceOf[MethodBuilder]
	if (!mBuilder.IsAbstract())
          try {
	    mcode = mBuilder.GetILGenerator()
          } catch {
            case e: Exception =>
              System.out.println("m.symbol       = " + Flags.flagsToString(m.symbol.flags) + " " + m.symbol)
              System.out.println("m.symbol.owner = " + Flags.flagsToString(m.symbol.owner.flags) + " " + m.symbol.owner)
              System.out.println("mBuilder       = " + mBuilder)
              System.out.println("mBuilder.DeclaringType = " +
                                 TypeAttributes.toString(mBuilder.DeclaringType.Attributes) +
                                 "::" + mBuilder.DeclaringType)
              throw e
          }
	  else
	    mcode = null
      }

      if (mcode != null) {
	for (val local <- (m.locals.diff(m.params))) {
	  if (settings.debug.value)
	    log("add local var: " + local + ", of kind " + local.kind)
	  val t: MsilType = msilType(local.kind)
	  val localBuilder = mcode.DeclareLocal(t)
	  localBuilder.SetLocalSymInfo(msilName(local.sym))
	  localBuilders(local) = localBuilder
	}
	genCode(m)
      }

    }

    var linearization: List[BasicBlock] = Nil
    // a "ret" instruction is needed (which is not present in
    //  icode) if there's no code after a try-catch block
    var needAdditionalRet: Boolean = false

    def genCode(m: IMethod): Unit = {

      code = m.code

      labels.clear
      linearization = linearizer.linearize(m)
      val orderedBlocks = (if (m.exh != Nil) orderBlocksForExh(linearization, m.exh)
			   else linearization)

      makeLabels(orderedBlocks) // orderBlocksForExh may create new Blocks -> new Labels
      genBlocks(orderedBlocks)
      if (needAdditionalRet) {
	mcode.Emit(OpCodes.Ret)
	needAdditionalRet = false
      }
    }

    abstract class ExHInstruction(handler: ExceptionHandler) { }
    case class BeginExceptionBlock(handler: ExceptionHandler) extends ExHInstruction(handler)
    case class BeginCatchBlock(handler: ExceptionHandler, exceptionType: MsilType) extends ExHInstruction(handler)
    case class BeginFinallyBlock(handler: ExceptionHandler) extends ExHInstruction(handler)
    case class EndExceptionBlock(handler: ExceptionHandler) extends ExHInstruction(handler)


    abstract class Block {
      var closed: Boolean = false
      def parentBlockList: Option[BlockList0]
      def firstBasicBlock: BasicBlock
      def lastBasicBlock: BasicBlock
//      def getExceptionBlock(exh: ExceptionHandler): Option[ExceptionBlock]
      def close(): Unit
/*      protected def findExceptionBlock(list: List[Block], exh: ExceptionHandler): Option[ExceptionBlock] = {
	var res: Option[ExceptionBlock] = None
	var i: Int = 0
	while (i < list.length && res == None) {
	  val b = list(i)
	  val exB = b.getExceptionBlock(exh)
	  exB match {
	    case some: Some[ExceptionBlock] => res = some
	    case None => ()
	  }
	  i = i + 1
	}
	res
      } */
    }
    case class CodeBlock(parent: BlockList0) extends Block {
      var basicBlocks: List[BasicBlock] = Nil
      def isEmpty = basicBlocks.isEmpty
      override def firstBasicBlock: BasicBlock = {
	if(isEmpty) null
	else {
	  if (closed) basicBlocks.head
	  else basicBlocks.last
	}
      }
      override def lastBasicBlock: BasicBlock = {
	if(isEmpty) null
	else {
	  if (closed) basicBlocks.last
	  else basicBlocks.head
	}
      }
      override def parentBlockList = Some(parent)
//      override def getExceptionBlock(exh: ExceptionHandler): Option[ExceptionBlock] = None
      override def close(): Unit = {
	basicBlocks = basicBlocks.reverse
	closed = true
      }
      override def toString() = {
	var res = ""
	res = res + TopBlock.indent + "CodeBlock(" + basicBlocks + ")\n"
	res
      }
    }
    abstract class BlockList0 extends Block {
      var blocks: List[Block] = Nil
      override def firstBasicBlock: BasicBlock = {
	if(blocks.isEmpty) null
	else {
	  if (closed) blocks.head.firstBasicBlock
	  else blocks.last.firstBasicBlock
	}
      }
      override def lastBasicBlock: BasicBlock = {
	if(blocks.isEmpty) null
	else {
	  if (closed) blocks.last.lastBasicBlock
	  else blocks.head.lastBasicBlock
	}
      }
/*      override def getExceptionBlock(exh: ExceptionHandler): Option[ExceptionBlock] = {
	findExceptionBlock(blocks, exh)
      } */
      def addExceptionBlock(exh: ExceptionHandler) = {
	if (settings.debug.value)
	  log("new exc block with " + exh + " to " + this)
	val e = new ExceptionBlock(this, exh)
	blocks = e :: blocks
	e
      }
      def addBasicBlock(bb: BasicBlock) = {
	if (settings.debug.value)
	  log("adding bb " + bb + " to " + this)
	var cb: CodeBlock = if (!blocks.isEmpty) {
	  blocks.head match {
	    case blk: CodeBlock => blk
	    case _ => null
	  }
	} else null
	if (cb == null) {
	  cb = new CodeBlock(this)
	  blocks = cb :: blocks
	}
	cb.basicBlocks = bb :: cb.basicBlocks
      }
      override def close(): Unit = {
	blocks.foreach(.close)
	blocks = blocks.reverse
	closed = true
      }
      override def toString() = {
	var res = ""
	res = res + TopBlock.indent + "BlockList0:\n"
	TopBlock.indent = TopBlock.indent + "  "
	for(val b <- blocks)
	  res = res + b + "\n"
	TopBlock.indent = TopBlock.indent.substring(0,TopBlock.indent.length-2)
	res
      }
    }
    case class BlockList(parent: Block) extends BlockList0 {
      override def parentBlockList: Option[BlockList0] = {
	if (parent == TopBlock)
	  Some(TopBlock)
	else parent match {
	  case bl: BlockList => Some(bl)
	  case cb: CatchBlock => Some(cb)
	  case _ => parent.parentBlockList
	}
      }
      override def toString() = {
	var res = ""
	res = res + TopBlock.indent + "BlockList:\n"
	res = res + super.toString()
	res
      }
    }
    case class ExceptionBlock(parent: Block, handler: ExceptionHandler) extends Block {
      var tryBlock: BlockList = new BlockList(this)
      var catchBlocks: List[CatchBlock] = Nil
      var finallyBlock: BlockList = new BlockList(this)
      override def firstBasicBlock = {
	tryBlock.firstBasicBlock
      }
      override def lastBasicBlock = {
	if (!finallyBlock.blocks.isEmpty)
	  finallyBlock.lastBasicBlock
	else if(!catchBlocks.isEmpty) {
	  if (closed) catchBlocks.last.lastBasicBlock
	  else catchBlocks.head.lastBasicBlock
	} else {
	  tryBlock.lastBasicBlock
	}
      }
      override def parentBlockList: Option[BlockList0] = {
	if (parent == TopBlock)
	  Some(TopBlock)
	else parent match {
	  case bl: BlockList => Some(bl)
	  case cb: CatchBlock => Some(cb)
	  case _ => parent.parentBlockList
	}
      }
/*      override def getExceptionBlock(exh: ExceptionHandler): Option[ExceptionBlock] = {
	if (exh == handler) Some(this)
	else {
	  val t = if (tryBlock == null) Nil else List(tryBlock)
	  val f = if (finallyBlock == null) Nil else List(finallyBlock)
	  findExceptionBlock(t ::: catchBlocks ::: f, exh)
	}
      }
*/
      def addCatchBlock(exSym: Symbol): CatchBlock = {
	if (settings.debug.value)
	  log("new catch block with " + exSym + " to " + this)
	val c = new CatchBlock(this, exSym)
	catchBlocks = c :: catchBlocks
	c
      }
      override def close(): Unit = {
	tryBlock.close
	catchBlocks.foreach(.close)
	catchBlocks = catchBlocks.reverse
	finallyBlock.close
	closed = true
      }
      override def toString() = {
	var res = ""
	res = res + TopBlock.indent + "ExceptionBlock, handler: " + handler + "\n"
	res = res + TopBlock.indent + "  " + "try:\n"
	TopBlock.indent = TopBlock.indent + "    "
	res = res + tryBlock + "\n"
	TopBlock.indent = TopBlock.indent.substring(0,TopBlock.indent.length-4)
	res = res + TopBlock.indent + "  " + "catch:\n"
	TopBlock.indent = TopBlock.indent + "    "
	for(val b <- catchBlocks)
	  res = res + b + "\n"
	TopBlock.indent = TopBlock.indent.substring(0,TopBlock.indent.length-4)
	res = res + TopBlock.indent + "  " + "finally:\n"
	TopBlock.indent = TopBlock.indent + "    "
	res = res + finallyBlock + "\n"
	TopBlock.indent = TopBlock.indent.substring(0,TopBlock.indent.length-4)
	res
      }
    }
    case class CatchBlock(parent: ExceptionBlock, exSym: Symbol) extends BlockList0 {
      override def parentBlockList: Option[BlockList0] = {
	parent.parentBlockList
      }
      override def toString() = {
	var res = ""
	res = res + TopBlock.indent + "CatchBlock:\n"
	res = res + super.toString()
	res
      }
    }
    case object TopBlock extends BlockList0 {
      var indent = ""
      override def parentBlockList = None
      override def toString() = {
	var res = ""
	res = res + TopBlock.indent + "TopBlock:\n"
	res = res + super.toString()
	res
      }
    }

    // for every basic block, a list of ExHInstructions to be executed:
    //   - Begin_ are executed before the block
    //   - EndExceptionBlock is executed after the block
    val bb2exHInstructions: HashMap[BasicBlock, List[ExHInstruction]] = new HashMap()
    // at the end of a try, catch or finally block, the jumps must not be emitted,
    // the automatically generated leave (or endfinally) will do the job.
    val omitJumpBlocks: HashSet[BasicBlock] = new HashSet()

    // suposes that finalizers are the same for different handlers
    // covering the same blocks
    def orderBlocksForExh(blocks: List[BasicBlock], exH: List[ExceptionHandler]): List[BasicBlock] = {

      var blocksToPut: List[BasicBlock] = blocks
      var nextBlock: BasicBlock = null
      var untreatedHandlers: List[ExceptionHandler] = exH
      TopBlock.blocks = Nil
      var currentBlock: BlockList0 = TopBlock
      def addBlocks(b: List[BasicBlock]):Unit = b match {
	case Nil => if (settings.debug.value) log("adding " + b)

	case x :: xs =>
	  if (settings.debug.value) log("adding " + b)
	  // problem: block may already be added, and and needs to be moved.
	  // if nextblock NOT in b: check if nextblock in blocksToPut, if NOT, check if movable, else don't put
	  if (nextBlock != null && b.contains(nextBlock)) {
	    val blocksToAdd = nextBlock :: b.diff(List(nextBlock))
	    nextBlock = null
	    addBlocks(blocksToAdd)
	  }
	  else if (untreatedHandlers.forall(h => !(h.covers(x)))) {

	    if (settings.debug.value) log(" no new handler for " + x)
	    if (untreatedHandlers.forall(h => !(h.blocks.contains(x) ||
						(h.finalizer != null &&
						 h.finalizer.covers(x)))))
	      {
		// the block is not part of some catch or finally code
		currentBlock.addBasicBlock(x)
		blocksToPut = blocksToPut.diff(List(x))
		if (settings.debug.value) log(" -> addBlocks(" + xs + ")")
		addBlocks(xs)
	      } else {
		if (settings.debug.value) log("x is part of catch or finally block")

		// check if the covered code of the handler x belongs to is empty
		// this check is not needed for finalizers: empty try with finalizer
		// is optimized by compiler (no try left)
		if(untreatedHandlers.forall(h =>
		  (!h.blocks.contains(x) || h.covered.isEmpty))) {
		    blocksToPut = blocksToPut.diff(List(x))
		    addBlocks(xs)
		  } else
		    addBlocks(xs ::: List(x))
	      }
	  } else { // there are new handlers for this block

	    var firstBlockAfter: HashMap[ExceptionHandler,BasicBlock] = new HashMap()
	    val savedCurrentBlock = currentBlock
	    /**
	     * the output blocks of this method are changed so that:
	     *  - only one block has a successor outside the set of blocks
	     *  - this block is the last of the reusulting list
	     *
	     * side-effect: it stores the successor in the hashMap
	     *  firstBlockAfter, which has to be emitted first after try/catch/finally,
	     *  because the target of the Leave-instruction will always be the first
	     *  instruction after EndExceptionBlock
	     *
	     * returns: the output blocks plus an Option containing the possibly created
	     * new block
	     **/
	    def adaptBlocks(blocks: List[BasicBlock], exh: ExceptionHandler): (List[BasicBlock], Option[BasicBlock]) = {
	      def outsideTargets(block: BasicBlock, blocks: List[BasicBlock]) = {
		block.successors.filter(scc => !blocks.contains(scc))
	      }
	      // get leaving blocks and their outside targets
	      def leavingBlocks(blocks: List[BasicBlock]): List[(BasicBlock, List[BasicBlock])] = {
		for {val b <- blocks
		     val t = outsideTargets(b, blocks)
		     t.length != 0 } yield (b, t)
	      }

	      def replaceOutJumps(blocks: List[BasicBlock], leaving: List[(BasicBlock, List[BasicBlock])], exh: ExceptionHandler): (List[BasicBlock], Option[BasicBlock]) = {
		def replaceJump(block: BasicBlock, from: BasicBlock, to: BasicBlock) = block.lastInstruction match {
		  case JUMP(where) =>
		    assert(from == where)
		  block.replaceInstruction(block.lastInstruction, JUMP(to))
		  case CJUMP(success, failure, cond, kind) =>
		    if (from == success)
		      block.replaceInstruction(block.lastInstruction, CJUMP(to, failure, cond, kind))
		    else
		      assert(from == failure)
		    if (from == failure)
		      block.replaceInstruction(block.lastInstruction, CJUMP(success, to, cond, kind))
		  case CZJUMP(success, failure, cond, kind) =>
		    if (from == success)
		      block.replaceInstruction(block.lastInstruction, CZJUMP(to, failure, cond, kind))
		    else
		      assert(from == failure)
		    if (from == failure)
		      block.replaceInstruction(block.lastInstruction, CZJUMP(success, to, cond, kind))
		  case SWITCH(tags, labels) => // labels: List[BasicBlock]
		    val newLabels = labels.map(b => if (b == from) to else b)
		    assert(newLabels.contains(to))
		    block.replaceInstruction(block.lastInstruction, SWITCH(tags, newLabels))
		  case _ => abort("expected branch at the end of block " + block)
		}

		val jumpOutBlock = blocks.last.code.newBlock
		jumpOutBlock.emit(JUMP(firstBlockAfter(exh)))
		jumpOutBlock.close
		leaving.foreach(p => {
		  val lBlock = p._1
		  val target = p._2(0) // the elemets of p._2 are all the same, checked before
		  replaceJump(lBlock, target, jumpOutBlock)
		})
		(blocks ::: List(jumpOutBlock), Some(jumpOutBlock))
	      }

	      val leaving = leavingBlocks(blocks)
	      if (leaving.length == 0)
		(blocks, None)
	      else if (leaving.length == 1) {
		val outside = leaving(0)._2
		assert(outside.forall(b => b == outside(0)), "exception-block leaving to multiple targets")
		if (!firstBlockAfter.isDefinedAt(exh))
		  firstBlockAfter(exh) = outside(0)
		else
		  assert(firstBlockAfter(exh) == outside(0), "try/catch leaving to multiple targets: " + firstBlockAfter(exh) + ", new: " + outside(0))
		val last = leaving(0)._1
		(blocks.diff(List(last)) ::: List(last), None)
	      } else {
		val outside = leaving.flatMap(p => p._2)
		assert(outside.forall(b => b == outside(0)), "exception-block leaving to multiple targets")
		if (!firstBlockAfter.isDefinedAt(exh))
		  firstBlockAfter(exh) = outside(0)
		else
		  assert(firstBlockAfter(exh) == outside(0), "try/catch leaving to multiple targets")
		replaceOutJumps(blocks, leaving, exh)
	      }
	    }

	    var affectedHandlers: List[ExceptionHandler] = Nil
	    untreatedHandlers.foreach( (h) => {
	      if (h.covers(x)){
		affectedHandlers = h :: affectedHandlers
	      }
	    })
	    affectedHandlers = affectedHandlers.filter(h => {h.covered.length == affectedHandlers(0).covered.length})
	    untreatedHandlers = untreatedHandlers.diff(affectedHandlers)

	    // shorter try-catch-finally last (the ones contained in another)
	    affectedHandlers = affectedHandlers.sort({(h1, h2) => h1.covered.length > h2.covered.length})

	    // more than one catch produces more than one exh, but we only need one
	    var singleAffectedHandler: ExceptionHandler = affectedHandlers(0) // List[ExceptionHandler] = Nil
	    var exceptionBlock: Option[ExceptionBlock] = None
	    affectedHandlers.foreach(h1 => {
	      val (adaptedBlocks, newBlock) = adaptBlocks(blocksToPut.intersect(h1.blocks), singleAffectedHandler)
	      newBlock match {
		case Some(block) =>
		  blocksToPut = blocksToPut ::: List(block)
		  h1.addBlock(block)
		case None => ()
	      }
	      val orderedCatchBlocks = h1.startBlock :: adaptedBlocks.diff(List(h1.startBlock))

	      exceptionBlock match {
		case Some(excBlock) =>
		  val catchBlock = excBlock.addCatchBlock(h1.cls)
		  currentBlock = catchBlock
		  addBlocks(orderedCatchBlocks)
		case None =>
		  val excBlock = currentBlock.addExceptionBlock(singleAffectedHandler)
		  exceptionBlock = Some(excBlock)

		  val (tryBlocks, newBlock) = adaptBlocks(blocksToPut.intersect(singleAffectedHandler.covered), singleAffectedHandler)

		  newBlock match {
		    case Some(block) =>
		      blocksToPut = blocksToPut ::: List(block)
		      singleAffectedHandler.addCoveredBlock(block)
		    case None => ()
		  }
		  currentBlock = excBlock.tryBlock
		  addBlocks(tryBlocks)

		  if (singleAffectedHandler.finalizer != null && singleAffectedHandler.finalizer != NoFinalizer) {
		    val (blocks0, newBlock) = adaptBlocks(blocksToPut.intersect(singleAffectedHandler.finalizer.blocks), singleAffectedHandler)
		    newBlock match {
		      case Some(block) =>
			blocksToPut = blocksToPut ::: List(block)
			singleAffectedHandler.finalizer.addBlock(block)
		      case None => ()
		    }
		    val blocks = singleAffectedHandler.finalizer.startBlock :: blocks0.diff(List(singleAffectedHandler.finalizer.startBlock))
		    currentBlock = excBlock.finallyBlock
		    addBlocks(blocks)
		  }

		  val catchBlock = excBlock.addCatchBlock(singleAffectedHandler.cls)
		  currentBlock = catchBlock
		  addBlocks(orderedCatchBlocks)
	      }
	      if (firstBlockAfter.isDefinedAt(singleAffectedHandler))
		nextBlock = firstBlockAfter(singleAffectedHandler)
	      else
		nextBlock = null
	    })

	    currentBlock = savedCurrentBlock

	    if (settings.debug.value)
	      log(" -> addBlocks(" + xs.intersect(blocksToPut) + ")")
	    addBlocks(xs.intersect(blocksToPut))
	  }
      }

      // begin method orderBlocksForExh

      if (settings.debug.value)
	log("before: " + blocks)
      // some blocks may have been removed by linearization
      untreatedHandlers.foreach(h => {
	h.blocks = h.blocks.intersect(blocksToPut)
	h.covered = h.covered.intersect(blocksToPut)
	if (h.finalizer != null && h.finalizer != NoFinalizer)
	  h.finalizer.blocks = h.finalizer.blocks.intersect(blocksToPut)
      })
      addBlocks(blocks)

      TopBlock.close()

      if (settings.debug.value) log("TopBlock tree is: ")
      if (settings.debug.value) Console.println(TopBlock)

      bb2exHInstructions.clear
      def addExHInstruction(b: BasicBlock, ehi: ExHInstruction) = {
	if (settings.debug.value)
	  log("adding exhinstr: " + b + " -> " + ehi)

	if (bb2exHInstructions.contains(b)){
	  bb2exHInstructions(b) = ehi :: bb2exHInstructions(b)
	} else {
	  bb2exHInstructions(b) = List(ehi)
	}
      }
      omitJumpBlocks.clear
      def omitJump(blk: BasicBlock) = {
	omitJumpBlocks += blk
      }
      var orderedBlocks: List[BasicBlock] = Nil
      def flatten(block: Block): Unit = {
	if (block == TopBlock) {
	  for (val b <- TopBlock.blocks) flatten(b)
	} else block match {
	  case cb: CodeBlock =>
	    orderedBlocks = orderedBlocks ::: cb.basicBlocks
	  case bl: BlockList =>
	    for (val b <- bl.blocks) flatten(b)
	  case cb: CatchBlock =>
	    for (val b <- cb.blocks) flatten(b)
	  case eb: ExceptionBlock =>
	    val handler = eb.handler
	    addExHInstruction(eb.tryBlock.firstBasicBlock, new BeginExceptionBlock(handler))
	    omitJump(eb.tryBlock.lastBasicBlock)
	    flatten(eb.tryBlock)
	    for(val c <- eb.catchBlocks) {
	      val t: MsilType = (if (c.exSym == NoSymbol) EXCEPTION
				 else getType(c.exSym))
	      addExHInstruction(c.firstBasicBlock, new BeginCatchBlock(handler, t))
	      omitJump(c.lastBasicBlock)
	      flatten(c)
	    }
	    if (handler.finalizer != null && handler.finalizer != NoFinalizer) {
	      addExHInstruction(eb.finallyBlock.firstBasicBlock, new BeginFinallyBlock(handler))
	      flatten(eb.finallyBlock)
	      addExHInstruction(eb.finallyBlock.lastBasicBlock, new EndExceptionBlock(handler))
	      omitJump(eb.finallyBlock.lastBasicBlock)
	    } else {
	      addExHInstruction(eb.catchBlocks.last.lastBasicBlock, new EndExceptionBlock(handler))
	    }
	}
      }

      flatten(TopBlock)

      assert(untreatedHandlers.forall((h) => h.covered.isEmpty),
	     "untreated exception handlers left: " + untreatedHandlers)
      // remove catch blocks from empty handlers (finally-blocks remain)
      untreatedHandlers.foreach((h) => {
	orderedBlocks = orderedBlocks.diff(h.blocks)
      })

      // take care of order in which exHInstructions are executed (BeginExceptionBlock as last)
      bb2exHInstructions.keys.foreach((b) => {
	bb2exHInstructions(b).sort((i1, i2) => (!i1.isInstanceOf[BeginExceptionBlock]))
      })


      if (settings.debug.value){
	log("after: " + orderedBlocks)
	log(" exhInstr: " + bb2exHInstructions)
      }

      orderedBlocks
    }

    var currentBlock: BasicBlock = _
    var lastBlock: BasicBlock = _
    var nextBlock: BasicBlock = _

    def genBlocks(l: List[BasicBlock]): Unit = l match {
      case Nil => ()
      case x :: Nil => currentBlock = x; nextBlock = null; genBlock(x)
      case x :: y :: ys => currentBlock = x; nextBlock = y; genBlock(x); genBlocks(y :: ys)
    }


    var ignoreNextDup: Boolean = false
    val excResultLocals: Stack[LocalBuilder] = new Stack()

    def genBlock(b: BasicBlock): Unit = {
      // at begin of the first block, there's nothing to save =>
      //  lastBlock != null is secure
      def saveResult(resType: MsilType) = if (resType != MVOID && lastBlock != null) {
	lastBlock.lastInstruction match {
	  case THROW() => ()
	  case _ =>
	    val lb: LocalBuilder = excResultLocals.top
	    mcode.Emit(OpCodes.Stloc, lb)
	}
      }

      if (bb2exHInstructions.contains(b)){
	bb2exHInstructions(b).foreach((i) => i match {
	  case BeginExceptionBlock(handler) =>
	    if (settings.debug.value) log("begin ex blk: " + handler)
	    mcode.BeginExceptionBlock()
	    val resType = msilType(handler.resultKind)
	    if (resType != MVOID) {
	      val l = mcode.DeclareLocal(resType)
	      l.SetLocalSymInfo("$exhResult")
	      excResultLocals.push(l)
	    }
	  case BeginCatchBlock(handler, exType) =>
	    if (settings.debug.value) log("begin catch blk: " + handler + ", tpe: " + exType)
	    saveResult(msilType(handler.resultKind))
	    mcode.BeginCatchBlock(exType)
	  case BeginFinallyBlock(handler) =>
	    saveResult(msilType(handler.resultKind))
	    mcode.BeginFinallyBlock()
	  case EndExceptionBlock(handler) => ()
	  case _ => abort("unknown case: " + i)
	})
      }

      mcode.MarkLabel(labels(b))
      if (settings.debug.value)
        log("Generating code for block: " + b)

      var lastLineNr: Int = 0

      b traverse ( instr => {

	needAdditionalRet = false

	var currentLineNr = try { clasz.cunit.position(instr.pos).line } catch {
          case _: Error =>
            log("Warning: wrong position in: " + method)
          lastLineNr
	} // if getting line number fails

	if (currentLineNr != lastLineNr) {
	  mcode.setPosition(currentLineNr)
	  lastLineNr = currentLineNr
	}

        instr match {
          case THIS(clasz) =>
            mcode.Emit(OpCodes.Ldarg_0)

          case CONSTANT(const) =>
            const.tag match {
              case UnitTag    => ()
              case BooleanTag => mcode.Emit(if (const.booleanValue) OpCodes.Ldc_I4_1
					    else OpCodes.Ldc_I4_0)
              case ByteTag    => loadI4(const.byteValue, mcode)
              case ShortTag   => loadI4(const.shortValue, mcode)
              case CharTag    => loadI4(const.charValue, mcode)
              case IntTag     => loadI4(const.intValue, mcode)
              case LongTag    => mcode.Emit(OpCodes.Ldc_I8, const.longValue)
              case FloatTag   => mcode.Emit(OpCodes.Ldc_R4, const.floatValue)
              case DoubleTag  => mcode.Emit(OpCodes.Ldc_R8, const.doubleValue)
              case StringTag  => mcode.Emit(OpCodes.Ldstr, const.stringValue)
              case NullTag    => mcode.Emit(OpCodes.Ldnull)
              case ClassTag   =>
                mcode.Emit(OpCodes.Ldtoken, msilType(const.typeValue))
                mcode.Emit(OpCodes.Call, TYPE_FROM_HANDLE)
              case _          => abort("Unknown constant value: " + const)
            }

          case LOAD_ARRAY_ITEM(kind) =>
	    (kind: @unchecked) match {
	      case BOOL           => mcode.Emit(OpCodes.Ldelem_I1)
	      case BYTE           => mcode.Emit(OpCodes.Ldelem_U1)
	      case SHORT          => mcode.Emit(OpCodes.Ldelem_I2)
	      case CHAR           => mcode.Emit(OpCodes.Ldelem_U2)
	      case INT            => mcode.Emit(OpCodes.Ldelem_I4)
	      case LONG           => mcode.Emit(OpCodes.Ldelem_I8)
	      case FLOAT          => mcode.Emit(OpCodes.Ldelem_R4)
	      case DOUBLE         => mcode.Emit(OpCodes.Ldelem_R8)
	      case REFERENCE(cls) => mcode.Emit(OpCodes.Ldelem_Ref)

	      // case ARRAY(elem) is not possible, for Array[Array[Int]], the
	      //  load will be case REFERENCE(java.lang.Object)

	      // case UNIT is not possible: an Array[Unit] will be an
	      //  Array[scala.runtime.BoxedUnit] (-> case REFERENCE)
	    }

          case LOAD_LOCAL(local) =>
	    if (settings.debug.value)
	      log("load_local for " + local)
	    val isArg: Boolean = local.arg
	    val i = local.index
            if (isArg) {
	      loadArg(mcode)(i)
	    }
	    else {
	      loadLocal(i, local, mcode)
	    }

          case LOAD_FIELD(field, isStatic) =>
            if (settings.debug.value)
              log("LOAD_FIELD with owner: " + field.owner +
		  " flags: " + Flags.flagsToString(field.owner.flags))

	    var fieldInfo: FieldInfo = fields.get(field) match {
	      case Some(fInfo) => fInfo
	      case None =>
		val fInfo = getType(field.owner).GetField(msilName(field))
		fields(field) = fInfo
                fInfo
	    }
	    mcode.Emit(if (isStatic) OpCodes.Ldsfld else OpCodes.Ldfld, fieldInfo)


          case LOAD_MODULE(module) =>
            if (settings.debug.value)
              log("genearting LOAD_MODULE for: " + showsym(module))
	    mcode.Emit(OpCodes.Ldsfld, getModuleInstanceField(module))

          case STORE_ARRAY_ITEM(kind) =>
	    (kind: @unchecked) match {
	      case BOOL           => mcode.Emit(OpCodes.Stelem_I1)
	      case BYTE           => mcode.Emit(OpCodes.Stelem_I1)
	      case SHORT          => mcode.Emit(OpCodes.Stelem_I2)
	      case CHAR           => mcode.Emit(OpCodes.Stelem_I2)
	      case INT            => mcode.Emit(OpCodes.Stelem_I4)
	      case LONG           => mcode.Emit(OpCodes.Stelem_I8)
	      case FLOAT          => mcode.Emit(OpCodes.Stelem_R4)
	      case DOUBLE         => mcode.Emit(OpCodes.Stelem_R8)
	      case REFERENCE(cls) => mcode.Emit(OpCodes.Stelem_Ref)

	      // case UNIT / ARRRAY are not possible (see comment at LOAD_ARRAY_ITEM)
	    }

          case STORE_LOCAL(local) =>
	    val isArg: Boolean = local.arg
	    val i = local.index
	    if (settings.debug.value)
	      log("store_local for " + local + ", index " + i)

	    // there are some locals defined by the compiler that
	    // are isArg and are need to be stored.
            if (isArg) {
	      if (i >= -128 && i <= 127)
		mcode.Emit(OpCodes.Starg_S, i)
	      else
		mcode.Emit(OpCodes.Starg, i)
	    } else {
	      i match {
		case 0 => mcode.Emit(OpCodes.Stloc_0)
		case 1 => mcode.Emit(OpCodes.Stloc_1)
		case 2 => mcode.Emit(OpCodes.Stloc_2)
		case 3 => mcode.Emit(OpCodes.Stloc_3)
		case _      =>
		  if (i >= -128 && i <= 127)
		    mcode.Emit(OpCodes.Stloc_S, localBuilders(local))
		  else
		    mcode.Emit(OpCodes.Stloc, localBuilders(local))
	      }
	    }

          case STORE_FIELD(field, isStatic) =>
	    val fieldInfo: FieldInfo = fields.get(field) match {
	      case Some(fInfo) => fInfo
	      case None =>
		val fInfo = getType(field.owner).GetField(msilName(field))
		fields(field) = fInfo
                fInfo
	    }
	    mcode.Emit(if (isStatic) OpCodes.Stsfld else OpCodes.Stfld, fieldInfo)


          case CALL_PRIMITIVE(primitive) =>
            genPrimitive(primitive, instr.pos)


          case CALL_METHOD(msym, style) =>
	    if (msym.isClassConstructor) {
	      val constructorInfo: ConstructorInfo = getConstructor(msym)
	      (style: @unchecked) match {
		// normal constructor calls are Static..
		case Static(_) =>
                  if (method.symbol.isClassConstructor && method.symbol.owner == msym.owner)
                    mcode.Emit(OpCodes.Call, constructorInfo)
                  else
		    mcode.Emit(OpCodes.Newobj, constructorInfo)
		case SuperCall(_) =>
		  mcode.Emit(OpCodes.Call, constructorInfo)
		  if (isStaticModule(clasz.symbol) &&
		     notInitializedModules.contains(clasz.symbol))
		    {
		      notInitializedModules -= clasz.symbol
		      mcode.Emit(OpCodes.Ldarg_0)
		      mcode.Emit(OpCodes.Stsfld, getModuleInstanceField(clasz.symbol))
		    }
	      }

	    } else {
	      // java.lang.String.substring(int start_incl, int end_excl)
	      // System.String.Substring(int start_incl, int length)
	      if (msym == JSTRING_SUBSTRING_INT_INT) {
		val endLocal = mcode.DeclareLocal(MINT)
		endLocal.SetLocalSymInfo("$substring_end")
		mcode.Emit(OpCodes.Stloc, endLocal)
		mcode.Emit(OpCodes.Dup) // duplicate start_incl
		mcode.Emit(OpCodes.Neg)
		mcode.Emit(OpCodes.Ldloc, endLocal) // load end_excl
		mcode.Emit(OpCodes.Add) // compute length (-start + end)
	      }

	      var doEmit: Boolean = true
	      types.get(msym.owner) match {
                case Some(typ) if (typ.IsEnum) => {
		  def negBool = {
		    mcode.Emit(OpCodes.Ldc_I4_0)
		    mcode.Emit(OpCodes.Ceq)
		  }
		  doEmit = false
                  val name = msym.name
		  if (name eq nme.EQ)       { mcode.Emit(OpCodes.Ceq) }
                  else if (name eq nme.NE)  { mcode.Emit(OpCodes.Ceq); negBool }
                  else if (name eq nme.LT)  { mcode.Emit(OpCodes.Clt) }
                  else if (name eq nme.LE)  { mcode.Emit(OpCodes.Cgt); negBool }
                  else if (name eq nme.GT)  { mcode.Emit(OpCodes.Cgt) }
                  else if (name eq nme.GE)  { mcode.Emit(OpCodes.Clt); negBool }
                  else if (name eq nme.OR)  { mcode.Emit(OpCodes.Or) }
                  else if (name eq nme.AND) { mcode.Emit(OpCodes.And) }
                  else if (name eq nme.XOR) { mcode.Emit(OpCodes.Xor) }
                  else
		    doEmit = true
	        }
                case _ => ()
              }

	      // method: implicit view(FunctionX[PType0, PType1, ...,PTypeN, ResType]):DelegateType
	      val (isDelegateView, paramType, resType) = atPhase(currentRun.typerPhase){
		msym.tpe match {
		  case MethodType(parameterTypes, resultType)
		  if (parameterTypes.length == 1 && msym.name == nme.view_) =>
		    val isDel = definitions.isCorrespondingDelegate(resultType, parameterTypes(0))
		    (isDel, parameterTypes(0), resultType)
		  case _ => (false, null, null)
		}
	      }
	      if (doEmit && isDelegateView) {
		doEmit = false
		createDelegateCaller(paramType, resType)
	      }

	      if (doEmit &&
		  (msym.name == nme.PLUS || msym.name == nme.MINUS)
		  && clrTypes.isDelegateType(msilType(msym.owner.tpe)))
		{
		doEmit = false
		val methodInfo: MethodInfo = getMethod(msym)
		// call it as a static method, even if the compiler (symbol) thinks it's virtual
		mcode.Emit(OpCodes.Call, methodInfo)
		mcode.Emit(OpCodes.Castclass, msilType(msym.owner.tpe))
	      }

	      if (doEmit && definitions.Delegate_scalaCallers.contains(msym)) {
		doEmit = false
		val methodSym: Symbol = definitions.Delegate_scalaCallerTargets(msym)
		val delegateType: Type = msym.tpe match {
		  case MethodType(_, retType) => retType
		  case _ => abort("not a method type: " + msym.tpe)
		}
		val method: MethodInfo = getMethod(methodSym)
		val delegCtor = msilType(delegateType).GetConstructor(Array(MOBJECT, INT_PTR))
		if (methodSym.isStatic) {
		  mcode.Emit(OpCodes.Ldftn, method)
		} else {
		  mcode.Emit(OpCodes.Dup)
		  mcode.Emit(OpCodes.Ldvirtftn, method)
		}
		mcode.Emit(OpCodes.Newobj, delegCtor)
	      }

	      if (doEmit) {
		val methodInfo: MethodInfo = getMethod(msym)
		style match {
		  case SuperCall(_) =>
		    mcode.Emit(OpCodes.Call, methodInfo)
		  case Dynamic =>
		    mcode.Emit(if (dynToStatMapped(msym)) OpCodes.Call else OpCodes.Callvirt,
                               methodInfo)
                  case Static(_) =>
		    mcode.Emit(OpCodes.Call, methodInfo)
		}
	      }
	    }

          case BOX(boxType) => emitBox(mcode, boxType) //mcode.Emit(OpCodes.Box, msilType(boxType))

          case UNBOX(boxType) => emitUnbox(mcode, boxType)

          case NEW(REFERENCE(cls)) =>
	    ignoreNextDup = true

          // works also for arrays and reference-types
          case CREATE_ARRAY(elem) => mcode.Emit(OpCodes.Newarr, msilType(elem))

          // works for arrays and reference-types
          case IS_INSTANCE(tpe) =>
            mcode.Emit(OpCodes.Isinst, msilType(tpe))
            mcode.Emit(OpCodes.Ldnull)
            mcode.Emit(OpCodes.Ceq)
            mcode.Emit(OpCodes.Ldc_I4_0)
            mcode.Emit(OpCodes.Ceq)


	  // works for arrays and reference-types
	  // part from the scala reference: "S <: T does not imply
	  //  Array[S] <: Array[T] in Scala. However, it is possible
	  //  to cast an array of S to an array of T if such a cast
	  //  is permitted in the host environment."
          case CHECK_CAST(tpe) => mcode.Emit(OpCodes.Castclass, msilType(tpe))


	  // no SWITCH is generated when there's
	  //  - a default case ("case _ => ...") in the matching expr
	  //  - OR is used ("case 1 | 2 => ...")
          case SWITCH(tags, branches) =>
	    // tags is List[List[Int]]; a list of integers for every label.
	    //    if the int on stack is 4, and 4 is in the second list => jump
	    //    to second label
	    // branches is List[BasicBlock]
	    //    the labels to jump to (the last one ist the default one)

	    val switchLocal = mcode.DeclareLocal(MINT)
	    // several switch variables will appear with the same name in the
	    //  assembly code, but this makes no truble
	    switchLocal.SetLocalSymInfo("$switch_var")

	    mcode.Emit(OpCodes.Stloc, switchLocal)
	    var i: Int = 0
	    for(val l <- tags) {
	      var targetLabel = labels(branches(i))
	      for(val i <- l) {
		mcode.Emit(OpCodes.Ldloc, switchLocal)
		loadI4(i, mcode)
		mcode.Emit(OpCodes.Beq, targetLabel)
	      }
	      i = i + 1
	    }
	    val defaultTarget = labels(branches(i))
            if (nextBlock != defaultTarget && !omitJumpBlocks.contains(currentBlock))
	      mcode.Emit(OpCodes.Br, defaultTarget)


          case JUMP(where) =>
            if (nextBlock != where && !omitJumpBlocks.contains(currentBlock))
	      mcode.Emit(OpCodes.Br, labels(where))


          case CJUMP(success, failure, cond, kind) =>
	    // cond is TestOp (see Primitives.scala), and can take
	    // values EQ, NE, LT, GE LE, GT
	    // kind is TypeKind
	    val isFloat = kind == FLOAT || kind == DOUBLE
	    if (nextBlock == success || omitJumpBlocks.contains(currentBlock)) {
	      emitBr(cond.negate, labels(failure), isFloat)
	    } else {
	      emitBr(cond, labels(success), isFloat)
	      if (nextBlock != failure && !omitJumpBlocks.contains(currentBlock)) {
		mcode.Emit(OpCodes.Br, labels(failure))
	      }
	    }

          case CZJUMP(success, failure, cond, kind) =>
	    (kind: @unchecked) match {
	      case BOOL | REFERENCE(_) =>
		if (nextBlock == success || omitJumpBlocks.contains(currentBlock)) {
		  emitBrBool(cond.negate, labels(failure))
		} else {
		  emitBrBool(cond, labels(success))
		  if (nextBlock != failure && !omitJumpBlocks.contains(currentBlock)) {
		    mcode.Emit(OpCodes.Br, labels(failure))
		  }
		}
	    }

          case RETURN(kind) =>
	    mcode.Emit(OpCodes.Ret)

          case THROW() =>
	    mcode.Emit(OpCodes.Throw)

          case DROP(kind) =>
	    mcode.Emit(OpCodes.Pop)

          case DUP(kind) =>
	    // needed to create new instances
	    if (!ignoreNextDup) {
	      mcode.Emit(OpCodes.Dup)
	    } else {
	      ignoreNextDup = false
	    }


          case MONITOR_ENTER() =>
	    mcode.Emit(OpCodes.Call, MMONITOR_ENTER)

          case MONITOR_EXIT() =>
	    mcode.Emit(OpCodes.Call, MMONITOR_EXIT)

          case SCOPE_ENTER(_) | SCOPE_EXIT(_) => ()
        }

      }) // end b traverse instr => { .. }

      lastBlock = b // this way, saveResult knows lastBlock

      if (bb2exHInstructions.contains(b)){
	bb2exHInstructions(b).foreach((i) => i match {
	  case BeginExceptionBlock(handler) => ()
	  case BeginCatchBlock(handler, exType) => ()
	  case BeginFinallyBlock(handler) => ()
	  case EndExceptionBlock(handler) =>
	    if (settings.debug.value) log("end ex blk: " + handler)
	    val resType = msilType(handler.resultKind)
	    if (handler.finalizer == null || handler.finalizer == NoFinalizer)
	      saveResult(resType)
	    mcode.EndExceptionBlock()
	    if (resType != MVOID) {
	      val lb: LocalBuilder = excResultLocals.pop
	      mcode.Emit(OpCodes.Ldloc, lb)
	    } else
	      needAdditionalRet = true
	  case _ => abort("unknown case: " + i)
	})
      }

    } // end genBlock

    def genPrimitive(primitive: Primitive, pos: Int): Unit = {
      primitive match {
        case Negation(kind) =>
          kind match {
	    // CHECK: is ist possible to get this for BOOL? in this case, verify.
	    case BOOL | BYTE | CHAR | SHORT | INT | LONG | FLOAT | DOUBLE =>
	      mcode.Emit(OpCodes.Neg)

            case _ => abort("Impossible to negate a " + kind)
          }

        case Arithmetic(op, kind) =>
          op match {
            case ADD => mcode.Emit(OpCodes.Add)
            case SUB => mcode.Emit(OpCodes.Sub)
            case MUL => mcode.Emit(OpCodes.Mul)
            case DIV => mcode.Emit(OpCodes.Div)
            case REM => mcode.Emit(OpCodes.Rem)
            case NOT => mcode.Emit(OpCodes.Not) //bitwise complement (one's complement)
            case _ => abort("Unknown arithmetic primitive " + primitive )
          }

        case Logical(op, kind) => op match {
	  case AND => mcode.Emit(OpCodes.And)
	  case OR => mcode.Emit(OpCodes.Or)
	  case XOR => mcode.Emit(OpCodes.Xor)
	}


        case Shift(op, kind) => op match {
	  case LSL => mcode.Emit(OpCodes.Shl)
	  case ASR => mcode.Emit(OpCodes.Shr)
	  case LSR => mcode.Emit(OpCodes.Shr_Un)
	}


        case Conversion(src, dst) =>
          if (settings.debug.value)
            log("Converting from: " + src + " to: " + dst)

	dst match {
	  case BYTE =>   mcode.Emit(OpCodes.Conv_U1)
	  case SHORT =>  mcode.Emit(OpCodes.Conv_I2)
	  case CHAR =>   mcode.Emit(OpCodes.Conv_U2)
	  case INT =>    mcode.Emit(OpCodes.Conv_I4)
	  case LONG =>   mcode.Emit(OpCodes.Conv_I8)
	  case FLOAT =>  mcode.Emit(OpCodes.Conv_R4)
	  case DOUBLE => mcode.Emit(OpCodes.Conv_R8)
          case _ =>
	    Console.println("Illegal conversion at: " + clasz +
			    " at: " + method.sourceFile + ":" +
			    Position.line(clasz.cunit.source, pos))
	}

        case ArrayLength(_) =>
          mcode.Emit(OpCodes.Ldlen)

        case StartConcat =>
	  mcode.Emit(OpCodes.Newobj, MSTRING_BUILDER_CONSTR)


        case StringConcat(el) =>
          val elemType : MsilType = el match {
            case REFERENCE(_) | ARRAY(_) => MOBJECT
            case _ => msilType(el)
          }

	  val argTypes:Array[MsilType] = Array(elemType)
	  val stringBuilderAppend = MSTRING_BUILDER.GetMethod("Append", argTypes )
	  mcode.Emit(OpCodes.Callvirt,  stringBuilderAppend)

        case EndConcat =>
	  mcode.Emit(OpCodes.Callvirt, MSTRING_BUILDER_TOSTRING)

        case _ => abort("Unimplemented primitive " + primitive)
      }
    }


    ////////////////////// loading ///////////////////////

    def loadI4(value: Int, code: ILGenerator):Unit = value match {
      case -1 => code.Emit(OpCodes.Ldc_I4_M1)
      case 0  => code.Emit(OpCodes.Ldc_I4_0)
      case 1  => code.Emit(OpCodes.Ldc_I4_1)
      case 2  => code.Emit(OpCodes.Ldc_I4_2)
      case 3  => code.Emit(OpCodes.Ldc_I4_3)
      case 4  => code.Emit(OpCodes.Ldc_I4_4)
      case 5  => code.Emit(OpCodes.Ldc_I4_5)
      case 6  => code.Emit(OpCodes.Ldc_I4_6)
      case 7  => code.Emit(OpCodes.Ldc_I4_7)
      case 8  => code.Emit(OpCodes.Ldc_I4_8)
      case _  =>
	if (value >= -128 && value <= 127)
	  code.Emit(OpCodes.Ldc_I4_S, value)
	else
	  code.Emit(OpCodes.Ldc_I4, value)
    }

    def loadArg(code: ILGenerator)(i: Int) = i match {
      case 0 => code.Emit(OpCodes.Ldarg_0)
      case 1 => code.Emit(OpCodes.Ldarg_1)
      case 2 => code.Emit(OpCodes.Ldarg_2)
      case 3 => code.Emit(OpCodes.Ldarg_3)
      case _      =>
	if (i >= -128 && i <= 127)
	  code.Emit(OpCodes.Ldarg_S, i)
	else
	  code.Emit(OpCodes.Ldarg, i)
    }

    def loadLocal(i: Int, local: Local, code: ILGenerator) = i match {
      case 0 => code.Emit(OpCodes.Ldloc_0)
      case 1 => code.Emit(OpCodes.Ldloc_1)
      case 2 => code.Emit(OpCodes.Ldloc_2)
      case 3 => code.Emit(OpCodes.Ldloc_3)
      case _      =>
	if (i >= -128 && i <= 127)
	  code.Emit(OpCodes.Ldloc_S, localBuilders(local))
	else
	  code.Emit(OpCodes.Ldloc, localBuilders(local))
    }

    ////////////////////// labels ///////////////////////


    val labels: HashMap[BasicBlock, Label] = new HashMap() // labels for branches

    def emitBr(condition: TestOp, dest: Label, isFloat: Boolean) = {
      condition match {
	case EQ => mcode.Emit(OpCodes.Beq, dest)
	case NE => mcode.Emit(OpCodes.Bne_Un, dest)
	case LT => mcode.Emit(if (isFloat) OpCodes.Blt_Un else OpCodes.Blt, dest)
	case GE => mcode.Emit(if (isFloat) OpCodes.Bge_Un else OpCodes.Bge, dest)
	case LE => mcode.Emit(if (isFloat) OpCodes.Ble_Un else OpCodes.Ble, dest)
	case GT => mcode.Emit(if (isFloat) OpCodes.Bgt_Un else OpCodes.Bgt, dest)
      }
    }

    def emitBrBool(cond: TestOp, dest: Label) = {
      cond match {
	// EQ -> Brfalse, NE -> Brtrue; this is because we come from
	// a CZJUMP. If the value on the stack is 0 (e.g. a boolen
	// method returned false), and we are in the case EQ, then
	// we need to emit Brfalse (EQ Zero means false). vice versa
        case EQ => mcode.Emit(OpCodes.Brfalse, dest)
        case NE => mcode.Emit(OpCodes.Brtrue, dest)
      }
    }

    def makeLabels(bs: List[BasicBlock]) = {
      if (settings.debug.value)
        log("Making labels for: " + method)
      for (val bb <- bs) labels(bb) = mcode.DefineLabel()
    }

    ////////////////////// local vars ///////////////////////

    /**
     * Compute the indexes of each local variable of the given
     * method.
     */
    def computeLocalVarsIndex(m: IMethod): Unit = {
      val params = m.params
      var idx = 1
      if (isStaticSymbol(m.symbol))
        idx = 0

      for (val l <- params) {
        if (settings.debug.value)
          log("Index value for parameter " + l + ": " + idx)
        l.index = idx
        idx = idx + 1 // sizeOf(l.kind)
      }

      val locvars = m.locals.diff(params)
      idx = 0

      for (val l <- locvars) {
        if (settings.debug.value)
          log("Index value for local variable " + l + ": " + idx)
        l.index = idx
        idx = idx + 1 // sizeOf(l.kind)
      }

    }

    ////////////////////// Utilities ////////////////////////

    /** Return the a name of this symbol that can be used on the .NET
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
    def msilName(sym: Symbol): String = {
      val suffix: String = if (sym.hasFlag(Flags.MODULE) && !sym.isMethod &&
                               !sym.isImplClass &&
                               !sym.hasFlag(Flags.JAVA)) "$" else ""
      // Flags.JAVA: "symbol was not defined by a scala-class" (java, or .net-class)

      if (sym == definitions.AllClass)
        return "scala.All$"
      else if (sym == definitions.AllRefClass)
        return "scala.AllRef$"

      (if (sym.isClass || (sym.isModule && !sym.isMethod))
	sym.fullNameString
       else
	 sym.simpleName.toString().trim()) + suffix
    }


    ////////////////////// flags ///////////////////////

    def msilTypeFlags(sym: Symbol): Int = {
      var mf: Int = TypeAttributes.AutoLayout | TypeAttributes.AnsiClass

      mf = mf | (if (sym hasFlag Flags.PRIVATE) TypeAttributes.NotPublic else TypeAttributes.Public)
      mf = mf | (if (sym hasFlag Flags.ABSTRACT) TypeAttributes.Abstract else 0)
      mf = mf | (if (sym.isTrait && !sym.isImplClass) TypeAttributes.Interface else TypeAttributes.Class)
      mf = mf | (if (sym isFinal) TypeAttributes.Sealed else 0)

      sym.attributes foreach { a => a match {
	case AnnotationInfo(SerializableAttr, _, _) =>
	  // TODO: add the Serializable TypeAttribute also if the attribute
	  // System.SerializableAttribute is present (.net attribute, not scala)
	  //  Best way to do it: compare with
	  //  definitions.getClass("System.SerializableAttribute").tpe
	  //  when frontend available
	  mf = mf | TypeAttributes.Serializable
	case _ => ()
      }}

      mf
      // static: not possible (or?)
    }

    def msilMethodFlags(sym: Symbol): Short = {
      var mf: Int = MethodAttributes.HideBySig |
        (if (sym hasFlag Flags.PRIVATE) MethodAttributes.Private
	 else if (sym hasFlag Flags.PROTECTED) MethodAttributes.Family
	 else MethodAttributes.Public)

      if (!sym.isClassConstructor) {
        if (isStaticSymbol(sym))
	  mf = mf | FieldAttributes.Static
        else {
	  mf = mf | MethodAttributes.Virtual
	  if (sym.isFinal && !types(sym.owner).IsInterface)
	    mf = mf | MethodAttributes.Final
	  if (sym.hasFlag(Flags.DEFERRED) || types(sym.owner).IsInterface)
	    mf = mf | MethodAttributes.Abstract
        }
      }

      mf.toShort
    }

    def msilFieldFlags(sym: Symbol): Short = {
      var mf: Int =
        if (sym hasFlag Flags.PRIVATE) FieldAttributes.Private
	else if (sym hasFlag Flags.PROTECTED) FieldAttributes.FamORAssem
	else FieldAttributes.Public

      if (sym hasFlag Flags.FINAL)
	mf = mf | FieldAttributes.InitOnly

      if (isStaticSymbol(sym))
	mf = mf | FieldAttributes.Static

      // TRANSIENT: "not nerialized", VOLATILE: doesn't exist on .net
      // TODO: add this attribute also if the class has the custom attribute
      // System.NotSerializedAttribute
      sym.attributes.foreach( a => a match {
        case AnnotationInfo(TransientAtt, _, _) =>
	  mf = mf | FieldAttributes.NotSerialized
        case _ => ()
      })

      mf.toShort
    }


    def isStaticSymbol(s: Symbol): Boolean =
      s.hasFlag(Flags.STATIC) || s.hasFlag(Flags.STATICMEMBER) || s.owner.isImplClass


    ////////////////////// builders, types ///////////////////////

    var entryPoint: Symbol = _

    val notInitializedModules: HashSet[Symbol] = new HashSet()

    // TODO: create fields also in def createType, and not in genClass,
    // add a getField method (it only works as it is because fields never
    // accessed from outside a class)

    val localBuilders: HashMap[Local, LocalBuilder] = new HashMap()

    private[GenMSIL] def findEntryPoint(cls: IClass) = {
      def isEntryPoint(sym: Symbol):Boolean = {
        if (isStaticModule(sym.owner) && msilName(sym) == "main")
	  if (sym.tpe.paramTypes.length == 1) {
	    toTypeKind(sym.tpe.paramTypes(0)) match {
	      case ARRAY(elem) =>
	        if (elem.toType.symbol == definitions.StringClass){
		  return true
	        }
	      case _ => ()
	    }
	  }
        false
      }

      for (val m <- cls.methods) {
	if (isEntryPoint(m.symbol)) {
	  if (entryPoint == null)
	    entryPoint = m.symbol
	}
      }
      if (firstSourceName == "")
	if (cls.symbol.sourceFile != null) // is null for nested classes
	  firstSourceName = cls.symbol.sourceFile.name
    }

    // #####################################################################
    // get and create types

    private def msilType(t: TypeKind): MsilType = (t: @unchecked) match {
      case UNIT           => MVOID
      case BOOL           => MBOOL
      case BYTE           => MBYTE
      case SHORT          => MSHORT
      case CHAR           => MCHAR
      case INT            => MINT
      case LONG           => MLONG
      case FLOAT          => MFLOAT
      case DOUBLE         => MDOUBLE
      case REFERENCE(cls) => getType(cls)
      case ARRAY(elem)    => clrTypes.mkArrayType(msilType(elem))
    }

    private def msilType(tpe: Type): MsilType = msilType(toTypeKind(tpe))

    private def msilParamTypes(sym: Symbol): Array[MsilType] = {
      sym.tpe.paramTypes.map(msilType).toArray
    }

    def getType(sym: Symbol): MsilType = types.get(sym) match {
      case Some(typ) => typ
      case None =>
        val name = if (sym.isModuleClass && !sym.isTrait) sym.fullNameString + "$"
                   else sym.fullNameString
        val typ = clrTypes.getType(name)
        if (typ == null)
          throw new Error(showsym(sym) + " with name " + name)
        else {
          clrTypes.types(sym) = typ
          typ
        }
    }

    def mapType(sym: Symbol, mType: MsilType) = {
      assert(mType != null, showsym(sym))
      types(sym) = mType
    }

    def createTypeBuilder(iclass: IClass): Unit = {
      def getMsilType(tpe: Type): MsilType = {
        val sym = tpe.symbol
        types.get(sym) match {
          case Some(mtype) => mtype
          case None => createTypeBuilder(classes(sym)); types(sym)
        }
      }

      val sym = iclass.symbol
      if (types contains sym) return

      def isInterface(s: Symbol) = s.isTrait && !s.isImplClass
      val parents: List[Type] =
        if (sym.info.parents.isEmpty) List(definitions.ObjectClass.tpe)
        else sym.info.parents.removeDuplicates

      val superType = if (isInterface(sym)) null else getMsilType(parents.head)
      if (settings.debug.value)
	log("super type: " + parents(0).symbol + ", msil type: " + superType)

      val interfaces: Array[MsilType] = parents.tail.map(getMsilType).toArray
      if (parents.length > 1) {
	if (settings.debug.value){
	  log("interfaces:")
	  for(val i <- Iterator.range(0, interfaces.length)){
	    log("  type: " + parents(i + 1).symbol + ", msil type: " + interfaces(i))
	  }
	}
      }

      //TODO here: if the class is not top-level, use DefineNestedType?
      val tBuilder =
        mmodule.DefineType(msilName(sym), msilTypeFlags(sym), superType, interfaces)
      mapType(sym, tBuilder)
    } // createTypeBuilder


    def createClassMembers(iclass: IClass): Unit = try {
      createClassMembers0(iclass)
    }
    catch {
      case e: Throwable =>
        System.err.println(showsym(iclass.symbol))
        System.err.println("with methods = " + iclass.methods)
        throw e
    }

    def createClassMembers0(iclass: IClass): Unit = {
      val mtype = getType(iclass.symbol).asInstanceOf[TypeBuilder]
      for (val ifield <- iclass.fields) {
        val sym = ifield.symbol
        if (settings.debug.value)
          log("Adding field: " + sym.fullNameString)

        var attributes = msilFieldFlags(sym)
        val fBuilder = mtype.DefineField(msilName(sym), msilType(sym.tpe), attributes)
        fields(sym) = fBuilder
        addAttributes(fBuilder, sym.attributes)
      }

      if (iclass.symbol != definitions.ArrayClass)
      for (val m: IMethod <- iclass.methods) {
        val sym = m.symbol
        if (settings.debug.value)
          log("Creating MethodBuilder for " + Flags.flagsToString(sym.flags) + " " +
              sym.owner.fullNameString + "::" + sym.name)

        val ownerType = getType(sym.enclClass).asInstanceOf[TypeBuilder]
        assert(mtype == ownerType, "mtype = " + mtype + "; ownerType = " + ownerType)
        var paramTypes = msilParamTypes(sym)
        val attr = msilMethodFlags(sym)

        if (m.symbol.isClassConstructor) {
	  val constr =
            ownerType.DefineConstructor(attr, CallingConventions.Standard, paramTypes)
          for (val i <- Iterator.range(0, paramTypes.length)) {
	    constr.DefineParameter(i, ParameterAttributes.None, msilName(m.params(i).sym))
	  }
	  mapConstructor(sym, constr)
	  addAttributes(constr, sym.attributes)
        } else {
	  var resType = msilType(m.returnType)
	  val method =
            ownerType.DefineMethod(getMethodName(sym), attr, resType, paramTypes)
          for (val i <- Iterator.range(0, paramTypes.length)){
	    method.DefineParameter(i, ParameterAttributes.None, msilName(m.params(i).sym))
	  }
          if (!methods.contains(sym))
	    mapMethod(sym, method)
	  addAttributes(method, sym.attributes)
          if (settings.debug.value)
            log("\t created MethodBuilder " + method)
        }
      }

      if (isStaticModule(iclass.symbol)){
	addModuleInstanceField(iclass.symbol)
	notInitializedModules += iclass.symbol
	addStaticInit(iclass.symbol)
      }

    } // createClassMembers

    private def isTopLevelModule(sym: Symbol): Boolean =
      atPhase (currentRun.refchecksPhase) {
        sym.isModuleClass && !sym.isImplClass && !sym.isNestedClass
      }

    private def isStaticModule(sym: Symbol): Boolean = {
      sym.isModuleClass && !sym.isImplClass && !sym.hasFlag(Flags.LIFTED)
    }

    private def isCloneable(sym: Symbol): Boolean = {
      !sym.attributes.forall( a => a match {
	case AnnotationInfo(CloneableAttr, _, _) => false
	case _ => true
      })
    }

    private def addModuleInstanceField(sym: Symbol): Unit = {
      if (settings.debug.value)
	log("Adding Module-Instance Field for " + showsym(sym))
      val tBuilder = getType(sym).asInstanceOf[TypeBuilder]
      val fb = tBuilder.DefineField(MODULE_INSTANCE_NAME,
			   tBuilder,
			   (FieldAttributes.Public |
			    //FieldAttributes.InitOnly |
			    FieldAttributes.Static).toShort)
      fields(sym) = fb
    }


    // the symbol may be a object-symbol (module-symbol), or a module-class-symbol
    private def getModuleInstanceField(sym: Symbol): FieldInfo = {
      assert(sym.isModule || sym.isModuleClass, "Expected module: " + showsym(sym))

      // when called by LOAD_MODULE, the corresponding type maybe doesn't
      // exist yet -> make a getType
      val moduleClassSym = if (sym.isModule) sym.moduleClass else sym

      // TODO: get module field for modules not defined in the
      // source currently compiling (e.g. Console)

      fields get moduleClassSym match {
        case Some(sym) => sym
        case None =>
          //val mclass = types(moduleClassSym)
          val mClass = clrTypes.getType(moduleClassSym.fullNameString + "$")
          val mfield = mClass.GetField("MODULE$")
          assert(mfield ne null, "module not found " + showsym(moduleClassSym))
          fields(moduleClassSym) = mfield
          mfield
      }

      //fields(moduleClassSym)
    }

    private def addStaticInit(sym: Symbol): Unit = {
      val tBuilder = getType(sym).asInstanceOf[TypeBuilder]

      val staticInit = tBuilder.DefineConstructor(
	(MethodAttributes.Static | MethodAttributes.Public).toShort,
	CallingConventions.Standard,
	MsilType.EmptyTypes)

      val sicode = staticInit.GetILGenerator()

      val instanceConstructor = constructors(sym.primaryConstructor)

      sicode.Emit(OpCodes.Newobj, instanceConstructor)
      // the stsfld is done in the instance constructor, just after the super call.
      sicode.Emit(OpCodes.Pop)

      sicode.Emit(OpCodes.Ret)
    }


    private def dumpMirrorClass(sym: Symbol): Unit = {
      val tBuilder = getType(sym)
      assert(sym.isModuleClass, "Can't generate Mirror-Class for the Non-Module class " + sym)
      if (settings.debug.value)
        log("Dumping mirror class for object: " + sym)
      val moduleName = msilName(sym)
      val mirrorName = moduleName.substring(0, moduleName.length() - 1)
      val mirrorTypeBuilder = mmodule.DefineType(mirrorName,
						 TypeAttributes.Class |
						 TypeAttributes.Public |
						 TypeAttributes.Sealed,
		//FIXME: an object may have a super-type (a class, not an object) -> not in mirror class?
						 MOBJECT,
						 MsilType.EmptyTypes)

      for (val m <- sym.tpe.nonPrivateMembers;
           m.owner != definitions.ObjectClass && !m.hasFlag(Flags.PROTECTED) &&
           m.isMethod && !m.isClassConstructor && !isStaticSymbol(m) && !m.hasFlag(Flags.CASE))
	{
	  if (settings.debug.value)
            log("   Mirroring method: " + m)
          val paramTypes = msilParamTypes(m)
          val paramNames: Array[String] = new Array[String](paramTypes.length)
          for (val i <- Iterator.range(0, paramTypes.length))
            paramNames(i) = "x_" + i

	  // CHECK: verify if getMethodName is better than msilName
	  val mirrorMethod = mirrorTypeBuilder.DefineMethod(getMethodName(m),
							    MethodAttributes.Public |
							    MethodAttributes.Static,
							    msilType(m.tpe.resultType),
							    paramTypes)

	  var i = 0
	  while(i < paramTypes.length) {
	    mirrorMethod.DefineParameter(i, ParameterAttributes.None, paramNames(i))
	    i = i + 1
	  }

	  val mirrorCode = mirrorMethod.GetILGenerator()
	  mirrorCode.Emit(OpCodes.Ldsfld, getModuleInstanceField(sym))
          Iterator.range(0, paramTypes.length) foreach loadArg(mirrorCode)

	  mirrorCode.Emit(OpCodes.Call, getMethod(m))
	  mirrorCode.Emit(OpCodes.Ret)
	}

      addSymtabAttribute(sym.sourceModule, mirrorTypeBuilder)

      mirrorTypeBuilder.CreateType()
    }


    // #####################################################################
    // delegate callers

    var delegateCallers: TypeBuilder = _
    var nbDelegateCallers: Int = 0

    private def initDelegateCallers() = {
      delegateCallers = mmodule.DefineType("$DelegateCallers", TypeAttributes.Public |
					  TypeAttributes.Sealed)
    }

    private def createDelegateCaller(functionType: Type, delegateType: Type) = {
      if (delegateCallers == null)
	initDelegateCallers()
      // create a field an store the function-object
      val mFunctionType: MsilType = msilType(functionType)
      val anonfunField: FieldBuilder = delegateCallers.DefineField(
	"$anonfunField$$" + nbDelegateCallers, mFunctionType,
	FieldAttributes.InitOnly | FieldAttributes.Public | FieldAttributes.Static)
      mcode.Emit(OpCodes.Stsfld, anonfunField)


      // create the static caller method and the delegate object
      val (paramTypes, returnType) = delegateType.member(nme.apply).tpe match {
	case MethodType(delParams, delReturn) => (delParams, delReturn)
	case _ => abort("not a delegate type: "  + delegateType)
      }
      val caller: MethodBuilder = delegateCallers.DefineMethod(
	"$delegateCaller$$" + nbDelegateCallers,
	MethodAttributes.Final | MethodAttributes.Public | MethodAttributes.Static,
	msilType(returnType), paramTypes.map(msilType).toArray)
      for(val i <- Iterator.range(0, paramTypes.length))
	caller.DefineParameter(i, ParameterAttributes.None, "arg" + i)
      val delegCtor = msilType(delegateType).GetConstructor(Array(MOBJECT, INT_PTR))
      mcode.Emit(OpCodes.Ldnull)
      mcode.Emit(OpCodes.Ldftn, caller)
      mcode.Emit(OpCodes.Newobj, delegCtor)


      // create the static caller method body
      val functionApply: MethodInfo = getMethod(functionType.member(nme.apply))
      val dcode: ILGenerator = caller.GetILGenerator()
      dcode.Emit(OpCodes.Ldsfld, anonfunField)
      for(val i <- Iterator.range(0, paramTypes.length)) {
	loadArg(dcode)(i)
	emitBox(dcode, toTypeKind(paramTypes(i)))
      }
      dcode.Emit(OpCodes.Callvirt, functionApply)
      emitUnbox(dcode, toTypeKind(returnType))
      dcode.Emit(OpCodes.Ret)

      nbDelegateCallers = nbDelegateCallers + 1

    } //def createDelegateCaller

    def emitBox(code: ILGenerator, boxType: TypeKind) = (boxType: @unchecked) match {
      // doesn't make sense, unit as parameter..
      case UNIT   => code.Emit(OpCodes.Ldsfld, boxedUnit)
      case BOOL | BYTE | SHORT | CHAR | INT | LONG | FLOAT | DOUBLE =>
        code.Emit(OpCodes.Box, msilType(boxType))
      case REFERENCE(cls) if (definitions.unboxMethod.contains(cls)) =>
        code.Emit(OpCodes.Box, (msilType(boxType)))
      case REFERENCE(_) | ARRAY(_) => ()
    }

    def emitUnbox(code: ILGenerator, boxType: TypeKind) = (boxType: @unchecked) match {
      case UNIT   => code.Emit(OpCodes.Pop)
      case BOOL   => code.Emit(OpCodes.Unbox, MBOOL); code.Emit(OpCodes.Ldind_I1)
      case BYTE   => code.Emit(OpCodes.Call, toByte)
      case SHORT  => code.Emit(OpCodes.Call, toShort)
      case CHAR   => code.Emit(OpCodes.Call, toChar)
      case INT    => code.Emit(OpCodes.Call, toInt)
      case LONG   => code.Emit(OpCodes.Call, toLong)
      case FLOAT  => code.Emit(OpCodes.Call, toFloat)
      case DOUBLE => code.Emit(OpCodes.Call, toDouble)
      case REFERENCE(cls) if (definitions.unboxMethod.contains(cls)) =>
        code.Emit(OpCodes.Unbox, msilType(boxType))
        code.Emit(OpCodes.Ldobj, msilType(boxType))
      case REFERENCE(_) | ARRAY(_) => ()
    }

    // #####################################################################
    // get and create methods / constructors

    def getConstructor(sym: Symbol): ConstructorInfo = constructors.get(sym) match {
      case Some(constr) => constr
      case None =>
        val mClass = getType(sym.owner)
        val constr = mClass.GetConstructor(msilParamTypes(sym))
        if (constr eq null) {
          System.out.println("Cannot find constructor " + sym.owner + "::" + sym.name)
          System.out.println("scope = " + sym.owner.tpe.decls)
          throw new Error(sym.fullNameString)
        }
        else {
          mapConstructor(sym, constr)
          constr
        }
    }

    def mapConstructor(sym: Symbol, cInfo: ConstructorInfo) = {
      constructors(sym) = cInfo
    }

    private def getMethod(sym: Symbol): MethodInfo = {
    //private def getMethod(sym: Symbol): MethodInfo = sym match {
//       case SRToInt => toInt
//       case SRToDouble => toDouble
//       case SRToLong => toLong
//       case SRToChar => toChar
//       case SRToFloat => toFloat
//       case SRToBool => toBool
//       case SRToByte => toByte
//       case SRToShort => toShort
//       case _ =>

        methods.get(sym) match {
        case Some(method) => method
        case None =>
          val mClass = getType(sym.owner)
          try {
            val method = mClass.GetMethod(getMethodName(sym), msilParamTypes(sym),
                                          msilType(sym.tpe.resultType))
            if (method eq null) {
              System.out.println("Cannot find method " + sym.owner + "::" + msilName(sym))
              System.out.println("scope = " + sym.owner.tpe.decls)
              throw new Error(sym.fullNameString)
            }
            else {
              mapMethod(sym, method)
              method
            }
          }
          catch {
            case e: Exception =>
              Console.println("While looking up " + mClass + "::" + sym.nameString)
            Console.println("\t" + showsym(sym))
            throw e
          }
      }
    }

    /*
     * add a mapping between sym and mInfo
     */
    private def mapMethod(sym: Symbol, mInfo: MethodInfo):Unit = {
      assert (mInfo != null, mInfo)
      methods(sym) = mInfo
    }

    /*
     * add mapping between sym and method with newName, paramTypes of newClass
     */
    private def mapMethod(sym: Symbol, newClass: MsilType, newName: String, paramTypes: Array[MsilType]): Unit = {
      val methodInfo = newClass.GetMethod(newName, paramTypes)
      assert(methodInfo != null, "Can't find mapping for " + sym + " -> " +
	     newName + "(" + paramTypes + ")")
      mapMethod(sym, methodInfo)
      if (methodInfo.IsStatic)
        dynToStatMapped += sym
    }

    /*
     * add mapping between method with name and paramTypes of clazz to
     * method with newName and newParamTypes of newClass (used for instance
     * for "wait")
     */
    private def mapMethod(
      clazz: Symbol, name: Name, paramTypes: Array[Type],
      newClass: MsilType, newName: String, newParamTypes: Array[MsilType]):Unit = {
	val methodSym = lookupMethod(clazz, name, paramTypes)
	assert(methodSym != null, "cannot find method " + name + "(" +
	       paramTypes + ")" + " in class " + clazz)
	mapMethod(methodSym, newClass, newName, newParamTypes)
      }

    /*
     * add maping for member with name and paramTypes to member
     * newName of newClass (same parameters)
     */
    private def mapMethod(
      clazz: Symbol, name: Name, paramTypes: Array[Type],
      newClass: MsilType, newName: String):Unit = {
	mapMethod(clazz, name, paramTypes, newClass, newName, paramTypes map msilType)
      }

    /*
     * add mapping for all methods with name of clazz to the corresponding
     * method (same parameters) with newName of newClass
     */
    private def mapMethod(
      clazz: Symbol, name: Name,
      newClass: MsilType, newName: String):Unit = {
	val memberSym: Symbol = clazz.tpe.member(name)
	memberSym.tpe match {
	  // alternatives: List[Symbol]
	  case OverloadedType(_, alternatives) =>
	    alternatives.foreach(s => mapMethod(s, newClass, newName, msilParamTypes(s)))

	  // paramTypes: List[Type], resType: Type
	  case MethodType(paramTypes, resType) =>
	    mapMethod(memberSym, newClass, newName, msilParamTypes(memberSym))

	  case _ => abort("member not found: " + clazz + ", " + name)
	}
      }


    /*
     * find the method in clazz with name and paramTypes
     */
    private def lookupMethod(clazz: Symbol, name: Name, paramTypes: Array[Type]):Symbol = {
      val memberSym = clazz.tpe.member(name)
      memberSym.tpe match {
	case OverloadedType(_, alternatives) =>
	  alternatives.find(s => {
	    var i: Int = 0
	    var typesOK: Boolean = true
	    if (paramTypes.length == s.tpe.paramTypes.length){
	      while(i < paramTypes.length){
		if (paramTypes(i) != s.tpe.paramTypes(i))
		  typesOK = false
		i = i + 1
	      }
	    } else {
	      typesOK = false
	    }
	    typesOK
	  }) match {
	    case Some(sym) => sym
	    case None => abort("member of " + clazz + ", " + name + "(" +
			       paramTypes + ") not found")
	  }

	case MethodType(_, _) => memberSym

	case _ => abort("member not found: " + name + " of " + clazz)
      }
    }

    def getMethodName(methodSym: Symbol): String = {
      val name = methodSym.name
      val params = methodSym.tpe.paramTypes
      if (name == nme.finalize_ && params.length == 0)
	"Finalize"
      else if (name == nme.toString_ && params.length == 0)
	"ToString"
      else if (name == nme.hashCode_ && params.length == 0)
	"GetHashCode"
      else if (name == nme.equals_ && params.length == 1 &&
	      params(0) == definitions.ObjectClass.tpe)
	"Equals"
      // FIXME: why is there no nme.clone_ ?
      else if (name.toString() == "clone" && params.length == 0)
	"Clone"
      else
	msilName(methodSym)
    }

    private def showsym(sym: Symbol): String = (sym.toString +
      "\n  symbol = " + Flags.flagsToString(sym.flags) + " " + sym +
      "\n  owner  = " + Flags.flagsToString(sym.owner.flags) + " " + sym.owner
    )

  } // class BytecodeGenerator

} // class GenMSIL
