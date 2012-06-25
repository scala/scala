/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */


package ch.epfl.lamp.compiler.msil.emit

import ch.epfl.lamp.compiler.msil._
import ch.epfl.lamp.compiler.msil.util.Table
import java.util.Stack
import java.io.IOException
import ILGenerator._

/**
 * Generates Microsoft intermediate language (MSIL) instructions.
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
 final class ILGenerator(_owner: MethodBase) extends Visitable {

    //##########################################################################
    // public interface

    /**
     * Puts the specified instruction onto the stream of instructions.
     */
    def Emit(opcode: OpCode) {
	// switch opcode
        if (opcode == OpCode.Ret) {
	   emit(opcode, null, 0)
        } else {
           emit(opcode, null)
	}
    }

    /**
     * Puts the specified instruction and character argument onto
     * the Microsoft intermediate language (MSIL) stream of instructions.
     */
    def Emit(opcode: OpCode, arg: Char) {
	emit(opcode,new Character(arg))
    }

    /**
     * Puts the specified instruction and metadata token for the
     * specified constructor onto the Microsoft intermediate language
     * (MSIL) stream of instructions.
     */
    def Emit(opcode: OpCode, arg: ConstructorInfo) {
	assert(arg != null)
	// newobj
	// pop size is the number of parameters
	emit(opcode,arg, OpCode.PUSH_size(opcode.CEE_push) -
	     arg.GetParameters().length)
    }

    /**
     * Puts the specified instruction onto the Microsoft intermediate language (MSIL)
     * stream followed by the index of the given local variable.
     */
    def Emit(opcode: OpCode, arg: LocalBuilder) {
	assert(arg != null)
	// ldarg    | ldarg.s  | ldarga
	// ldarga.s  | ldloc    | ldloc.s  | ldloca
	// ldloca.s  | starg    | starg.s  | stloc
	// stloc.s

	// <instr_var> <localname>
	emit(opcode, arg)
    }


    /**
     * Puts the specified instruction and numerical argument onto
     * the Microsoft intermediate language (MSIL) stream of instructions.
     */
    def Emit(opcode: OpCode, arg: Double) {
	// ldc.r4 | ldc.r8
	emit(opcode, new java.lang.Double(arg))
    }

    /**
     * Puts the specified instruction and metadata token for the
     * specified field onto the Microsoft intermediate language (MSIL)
     * stream of instructions.
     */
    def Emit(opcode: OpCode,arg: FieldInfo) {
	assert(arg != null)
	// ldfld | ldflda | ldsfld | ldsflda | stfld | stsfld
	emit(opcode,arg)
    }

    /**
     * Puts the specified instruction and numerical argument onto
     * the Microsoft intermediate language (MSIL) stream of instructions.
     */
    def Emit(opcode: OpCode, arg: Short ) {
	emit(opcode, new java.lang.Short(arg))
    }

    /**
     * Puts the specified instruction and numerical argument onto
     * the Microsoft intermediate language (MSIL) stream of instructions.
     */
    def Emit(opcode: OpCode, arg: Int) {
	// ldc.i4 | ldc.i4.s | unaligned
	emit(opcode, new java.lang.Integer(arg))
    }

    /**
     * Puts the specified instruction and numerical argument onto
     * the Microsoft intermediate language (MSIL) stream of instructions.
     */
    def Emit(opcode: OpCode, arg: Long) {
	// ldc.i8
	emit(opcode, new java.lang.Long(arg))
    }

    /**
     * Puts the specified instruction onto the Microsoft intermediate
     * language (MSIL) stream and leaves space to include a label when
     * fixes are done.
     */
    def Emit(opcode: OpCode,label: Label) {
	assert(label != null)
	// beq    | beq.s    | bge    | bge.s    |
	// bge.un    | bge.un.s   | bgt    | bgt.s    | bgt.un | bgt.un.s |
	// ble       | ble.s      | ble.un | ble.un.s | blt    | blt.s    |
	// blt.un    | blt.un.s   | bne.un | bne.un.s | br     | br.s     |
	// brfalse   | brfalse.s  | brtrue | brtrue.s | leave  | leave.s

	emit(opcode, label)
	// is the label initialized ? if true backward jump else forward jump
	if (label.isInitialized()) {
// 	    if (arg.stacksize != lastLabel.stacksize) {
// 		System.err.println("ILGenerator.Emit: Stack depth differs depending on path:");
// 		System.err.println("\tmethod = " + owner);
// 		System.err.println("\tPC = 0x" + Table.short2hex(lastLabel.address));
// 	    }
	    //assert arg.stacksize == lastLabel.stacksize;
	}
	else {
	    label.setStacksize(lastLabel.getStacksize())
	}
    }

    /**
     * Puts the specified instruction onto the Microsoft intermediate
     * language (MSIL) stream and leaves space to include a label when
     * fixes are done.
     */
    def Emit(opcode: OpCode, arg: Array[Label] ) {
	assert(arg != null)
	// switch

	// <instr> ::= <instr_switch> ( <labels> )
	// Examples:
	// switch (0x3, -14, Label1)
	// switch (5, Label2)
	emit(opcode, arg, arg.length)
    }

    /**
     * Puts the specified instruction onto the Microsoft intermediate
     * language (MSIL) stream followed by the metadata token for the
     * given method.
     */
    def Emit(opcode: OpCode,arg: MethodInfo) {
	assert(arg != null)
	// call  | callvirt | jmp | ldftn | ldvirtftn
	// pop size is the number of parameters
	// pop 1 more if method is not static !
	// push size is either 0 (void Method) either 1
	assert(arg.ReturnType != null, "No ReturnType: " + arg.DeclaringType + "::" + arg.Name)

	val popush: Int = if (opcode == OpCode.Ldftn ||
            opcode == OpCode.Ldvirtftn ||
            opcode == OpCode.Jmp)
        {
           OpCode.PUSH_size(opcode.CEE_push) - OpCode.POP_size(opcode.CEE_pop)
        } else if (opcode == OpCode.Calli || opcode == OpCode.Callvirt) {
	        (if(arg.ReturnType == VOID) 0 else 1) - arg.GetParameters().length - 1
        } else {
	        (if(arg.ReturnType == VOID) 0 else 1) - arg.GetParameters().length
	}
	emit(opcode, arg, popush)
    }

    /**
     * Puts the specified instruction and numerical argument onto
     * the Microsoft intermediate language (MSIL) stream of instructions.
     */
    def Emit(opcode: OpCode, arg: Float ) {
	emit(opcode, new java.lang.Float(arg))
    }

    /**
     * Puts the specified instruction onto the Microsoft intermediate
     * language (MSIL) stream followed by the metadata token for the
     * given string.
     */
    def Emit(opcode: OpCode, arg: String ) {
	assert(arg != null)
	// ldstr
	emit(opcode, arg)
    }

    /**
     * Puts the specified instruction onto the Microsoft intermediate
     * language (MSIL) stream followed by the metadata token for the
     * given type.
     */
    def Emit(opcode: OpCode, arg: Type) {
	assert(arg != null)
	// box     | castclass | cpobj    | initobj | isinst    |
	// ldelema | ldobj     | mkrefany | newarr  | refanyval |
	// sizeof  | stobj     | unbox

	emit(opcode, arg)
    }

    /**
     * Puts a call or callvirt instruction onto the Microsoft intermediate
     * language (MSIL) stream.
     */
    def EmitCall(opcode: OpCode, arg: MethodInfo,
			 optionalParameterTypes: Array[Type]) {
	assert(arg != null)
	// pop size is the number of parameters
	// push size is either 0 (void Method) either 1
	//System.out.println(arg.ReturnType.Size + " " + arg.GetParameters().length);
	emit(opcode, arg, (if(arg.ReturnType == VOID) 0 else 1) -
	     arg.GetParameters().length)
    }

    /**
     * Emits the Microsoft intermediate language (MSIL) necessary to
     * call WriteLine with the given field.
     */
    def EmitWriteLine(arg: FieldInfo) {
	// first load field info
	// if static use OpCode.Ldsfld
	if (arg.IsStatic())
	    Emit(OpCodes.Ldsfld, arg)
	else
	    Emit(OpCodes.Ldfld, arg)
	// then call System.Console.WriteLine(arg.Type)
	val t: Type = Type.GetType("System.Console")
	val argsType: Array[Type] = new Array[Type](1)
	argsType(0) = arg.FieldType
	val m: MethodInfo = t.GetMethod("WriteLine", argsType)
	EmitCall(OpCode.Call, m, null)
    }

    /**
     * Emits the Microsoft intermediate language (MSIL) necessary
     * to call WriteLine with the given local variable.
     */
    def EmitWriteLine(arg: LocalBuilder) {
	// first load local variable
	Emit(OpCodes.Ldloc, arg)
	// then call System.Console.WriteLine(arg.Type)
	val t: Type = Type.GetType("System.Console")
	val argsType: Array[Type] = new Array[Type](1)
	argsType(0) = arg.LocalType
	val m: MethodInfo = t.GetMethod("WriteLine", argsType)
	EmitCall(OpCode.Call, m, null)
    }

    /**
     * Emits the Microsoft intermediate language (MSIL) to call
     * WriteLine with a string.
     */
    def EmitWriteLine(arg: String) {
	// first load string
	Emit(OpCode.Ldstr, arg)
	// then call System.Console.WriteLine(string)
	val t: Type = Type.GetType("System.Console")
	val argsType: Array[Type] = new Array[Type](1)
	argsType(0) = Type.GetType("System.String")
	val m: MethodInfo = t.GetMethod("WriteLine", argsType)
	EmitCall(OpCode.Call, m, null)
    }

    /**
     * Declares a local variable.
     */
    def DeclareLocal(localType: Type): LocalBuilder = {
	val l: LocalBuilder = new LocalBuilder(locals, localType)
    locals = locals + 1
      localList += l
	return l
    }

    /**
     * Returns a new label that can be used as a token for branching.
     * In order to set the position of the label within the stream, you
     * must call MarkLabel. This is just a token and does not yet represent
     * any particular location within the stream.
     */
    def DefineLabel():Label = {
	new Label.NormalLabel()
    }

    /**
     * Marks the Microsoft intermediate language (MSIL) stream's
     * current position with the given label.
     */
    def MarkLabel(label: Label) {
	label.mergeWith(lastLabel)
	/*
	label.address = lastLabel.address;
	//label.stacksize = lastLabel.stacksize;
	if (label.stacksize >= 0)
	    lastLabel.stacksize = label.stacksize;
	*/
    }

    /** Begins a lexical scope. */
    def BeginScope() {
	emitSpecialLabel(Label.NewScope)
    }

    /** Ends a lexical scope. */
    def EndScope() {
	emitSpecialLabel(Label.EndScope)
    }

    /**
     * Begins an exception block for a non-filtered exception.
     * The label for the end of the block. This will leave you in the correct
     * place to execute finally blocks or to finish the try.
     */
    def BeginExceptionBlock() {
        emitSpecialLabel(Label.Try)
        val endExc: Label = new Label.NormalLabel() // new Label(lastLabel) ???
        excStack.push(Label.Try, endExc)
    }

    /** Begins a catch block. */
    def BeginCatchBlock(exceptionType: Type) {
        val kind = excStack.peekKind()
        if (kind == Label.Kind.Try ||
            kind == Label.Kind.Catch) {
        /* ok */
        } else {
            throw new RuntimeException("Catch should follow either a try or catch")
	}
        val endExc: Label = excStack.popLabel()
	Emit(OpCodes.Leave, endExc)
	// the CLI automatically provide the exception object on the evaluation stack
	// we adjust the stacksize
	lastLabel.incStacksize()
        excStack.push(Label.Catch, endExc)
        emitSpecialLabel(Label.Catch, exceptionType)
    }

    /** Ends an exception block. */
    def EndExceptionBlock() {
        val kind = excStack.peekKind()
        if (kind == Label.Kind.Try) {
	    throw new RuntimeException("Try block with neither catch nor finally")
        } else if (kind == Label.Kind.Catch) {
	    Emit(OpCodes.Leave, excStack.peekLabel())
        } else if (kind == Label.Kind.Finally) {
	    Emit(OpCodes.Endfinally)
	}
        MarkLabel(excStack.popLabel())
        emitSpecialLabel(Label.EndTry)
    }

    /**
     * Begins a finally block in the Microsoft intermediate language
     * (MSIL) instruction stream.
     */
    def BeginFinallyBlock() {
        val endExc: Label = excStack.popLabel()
	Emit(OpCodes.Leave, endExc)
        excStack.push(Label.Finally, endExc)
	emitSpecialLabel(Label.Finally)
    }

    /**
     * Emits an instruction to throw an exception.
     */
    def ThrowException(exceptionType: Type) {
	assert(exceptionType != null)
	if (!exceptionType.isSubtypeOf(Type.GetType("System.Exception")))
	    throw new RuntimeException
		(exceptionType + " doesn't extend System.Exception" )
	val ctor: ConstructorInfo = exceptionType.GetConstructor(Type.EmptyTypes)
	if (ctor == null)
	    throw new RuntimeException("Type " + exceptionType
				       + "doesn't have a default constructor")
	Emit(OpCodes.Newobj, ctor)
	Emit(OpCodes.Throw)
    }

    /**
     * sets the line of the source file corresponding to the next instruction
     */
    def setPosition(line: Int) {
	    if (line != 0) lineNums.put(lastLabel, Integer.toString(line))
    }

    def setPosition(line: Int, filename: String) {
	    if (line != 0) lineNums.put(lastLabel, line + "  '" + filename + "'")
    }

    def setPosition(startLine: Int, endLine: Int, startCol: Int, endCol: Int, filename: String) {
      val lineRange = startLine + "," + endLine
      val colRange  = startCol  + "," + endCol
	  lineNums.put(lastLabel, lineRange + ":" + colRange + "  '" + filename + "'")
    }

   def getLocals(): Array[LocalBuilder] = localList.toArray

    def getLabelIterator() = labelList.iterator

    def getOpcodeIterator() = opcodeList.iterator

    def getArgumentIterator() = argumentList.iterator

    //##########################################################################
    // private implementation details



    // the local variable list
    private final val localList  = scala.collection.mutable.ArrayBuffer.empty[LocalBuilder]

    // the label list, the opcode list and the opcode argument list
    // labelList is an array of Label
    // opcodeList is an array of OpCode
    // argumentList is an array of Object (null if no argument)
    private final val labelList = scala.collection.mutable.ArrayBuffer.empty[Label]
    private final val opcodeList = scala.collection.mutable.ArrayBuffer.empty[OpCode]
    private final val argumentList = scala.collection.mutable.ArrayBuffer.empty[Object]

    // the program counter (pc)
    // also called the stream's current position
    private var pc: Int = 0

    // last label
    private var lastLabel: Label = new Label.NormalLabel(pc,0)

    // the maximum size of stack
    private var maxstack: Int = 0

    // the number of the locals
    private var locals: Int = 0

    // stack of label for exception mechanism
    private var excStack: ExceptionStack = new ExceptionStack()

    // the method info owner of this ILGenerator
    var owner: MethodBase = _owner

    val lineNums = scala.collection.mutable.Map.empty[Label, String]


    def getMaxStacksize(): Int = { this.maxstack }

    // private emit with Object Argument
    private def emit(opcode: OpCode, arg: Object) {
	emit(opcode, arg, opcode.CEE_popush)
    }

    // private emit with Object Argument and override POPUSH
    private def emit(opcode: OpCode, arg: Object, overridePOPUSH: Int) {
	// add label, opcode and argument
      labelList += lastLabel
      opcodeList += opcode
      argumentList += arg
	// compute new lastLabel (next label)
	val stackSize: Int = lastLabel.getStacksize() + overridePOPUSH
	if (stackSize < 0) {
          val msg = "ILGenerator.emit(): Stack underflow in method: " + owner
          scala.Console.println(msg)
          // throw new RuntimeException(msg)
	}
	if (stackSize > maxstack)
	    maxstack = stackSize
	var address: Int = lastLabel.getAddress() + opcode.CEE_length
        if (opcode.CEE_opcode == OpCode.CEE_SWITCH) {
            address = address + 4*arg.asInstanceOf[Array[Label]].length
        }
	lastLabel = new Label.NormalLabel(address, stackSize)
	pc = pc + 1
    }

   def Ldarg0WasJustEmitted() : Boolean = {
     if(opcodeList.isEmpty)
       return false
     val lastEmitted = opcodeList(opcodeList.size - 1)
     lastEmitted eq OpCode.Ldarg_0
   }

    private def emitSpecialLabel(l: Label) {
        emitSpecialLabel(l, null)
    }
    private def emitSpecialLabel(l: Label, catchType: Type) {
        labelList += l
        opcodeList += null
        argumentList += catchType
    }

    //##########################################################################
    //
    @throws(classOf[IOException])
    def apply(v: Visitor)  {
	v.caseILGenerator(this)
    }

    //##########################################################################
}  // class ILGenerator


object ILGenerator {

   val VOID: Type = Type.GetType("System.Void")
   val NO_LABEL: String = ""

    private final class ExceptionStack {
        private val labels = new scala.collection.mutable.Stack[Label]()
        private val kinds = new scala.collection.mutable.Stack[Label]()
        def ExceptionStack() {}
        def pop() { labels.pop; kinds.pop }
        def push(kind: Label, label: Label) {
            kinds.push(kind); labels.push(label)
        }
        def peekKind(): Label.Kind = kinds.top.getKind
        def peekLabel(): Label = labels.top
        def popLabel(): Label = { kinds.pop(); labels.pop() }
    }

}

