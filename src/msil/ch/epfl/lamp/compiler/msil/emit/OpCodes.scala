/*
 * System.Reflection.Emit-like API for writing .NET assemblies to MSIL
 */


package ch.epfl.lamp.compiler.msil.emit


/**
 * Provides field representations of the Microsoft Intermediate Language (MSIL)
 * instructions for emission by the ILGenerator class members (such as Emit).
 *
 * @author Nikolay Mihaylov
 * @version 1.0
 */
object OpCodes {

    //##########################################################################

    /**
     * Adds two values and pushes the result onto the evaluation stack.
     */
     final val Add = OpCode.Add

    /**
     * Fills space if bytecodes are patched. No meaningful operation is performed
     * although a processing cycle can be consumed.
     */
     final val Nop = OpCode.Nop

    /**
     * Signals the Common Language Infrastructure (CLI) to inform the debugger that
     * a break point has been tripped.
     */
     final val Break = OpCode.Break

    /**
     * Loads the argument at index 0 onto the evaluation stack.
     */
     final val Ldarg_0 = OpCode.Ldarg_0

    /**
     * Loads the argument at index 1 onto the evaluation stack.
     */
     final val Ldarg_1 = OpCode.Ldarg_1

    /**
     * Loads the argument at index 2 onto the evaluation stack.
     */
     final val Ldarg_2 = OpCode.Ldarg_2

    /**
     * Loads the argument at index 3 onto the evaluation stack.
     */
     final val Ldarg_3 = OpCode.Ldarg_3

    /**
     * Loads the local variable at index 0 onto the evaluation stack.
     */
     final val Ldloc_0 = OpCode.Ldloc_0

    /**
     * Loads the local variable at index 1 onto the evaluation stack.
     */
     final val Ldloc_1 = OpCode.Ldloc_1

    /**
     * Loads the local variable at index 2 onto the evaluation stack.
     */
     final val Ldloc_2 = OpCode.Ldloc_2

    /**
     * Loads the local variable at index 3 onto the evaluation stack.
     */
     final val Ldloc_3 = OpCode.Ldloc_3

    /**
     * Pops the current value from the top of the evaluation stack and
     * stores it in a the local variable list at index 0.
     */
     final val Stloc_0 = OpCode.Stloc_0

    /**
     * Pops the current value from the top of the evaluation stack and
     * stores it in a the local variable list at index 1.
     */
     final val Stloc_1 = OpCode.Stloc_1

    /**
     * Pops the current value from the top of the evaluation stack and
     * stores it in a the local variable list at index 2.
     */
     final val Stloc_2 = OpCode.Stloc_2

    /**
     * Pops the current value from the top of the evaluation stack and
     * stores it in a the local variable list at index 3.
     */
     final val Stloc_3 = OpCode.Stloc_3

    /**
     * Loads the argument (referenced by a specified short form index)
     * onto the evaluation stack.
     */
     final val Ldarg_S = OpCode.Ldarg_S

    /**
     * Load an argument address, in short form, onto the evaluation stack.
     */
     final val Ldarga_S = OpCode.Ldarga_S

    /**
     * Loads the local variable at a specific index onto the evaluation stack,
     * short form.
     */
     final val Ldloc_S = OpCode.Ldloc_S

    /**
     * Loads the address of the local variable at a specific index onto
     * the evaluation stack, short form.
     */
     final val Ldloca_S = OpCode.Ldloca_S

    /**
     * Stores the value on top of the evaluation stack in the argument slot
     * at a specified index, short form.
     */
     final val Starg_S = OpCode.Starg_S

    /**
     * Pops the current value from the top of the evaluation stack and stores it
     * in a the local variable list at index (short form).
     */
     final val Stloc_S = OpCode.Stloc_S

    /**
     * Pushes a null reference (type O) onto the evaluation stack.
     */
     final val Ldnull = OpCode.Ldnull

    /**
     * Pushes the integer value of -1 onto the evaluation stack as an int32.
     */
     final val Ldc_I4_M1 = OpCode.Ldc_I4_M1

    /**
     * Pushes the integer value of 0 onto the evaluation stack as an int32.
     */
     final val Ldc_I4_0 = OpCode.Ldc_I4_0

    /**
     * Pushes the integer value of 1 onto the evaluation stack as an int32.
     */
     final val Ldc_I4_1 = OpCode.Ldc_I4_1

    /**
     * Pushes the integer value of 2 onto the evaluation stack as an int32.
     */
     final val Ldc_I4_2 = OpCode.Ldc_I4_2

    /**
     * Pushes the integer value of 3 onto the evaluation stack as an int32.
     */
     final val Ldc_I4_3 = OpCode.Ldc_I4_3

    /**
     * Pushes the integer value of 4 onto the evaluation stack as an int32.
     */
     final val Ldc_I4_4 = OpCode.Ldc_I4_4

    /**
     * Pushes the integer value of 5 onto the evaluation stack as an int32.
     */
     final val Ldc_I4_5 = OpCode.Ldc_I4_5

    /**
     * Pushes the integer value of 6 onto the evaluation stack as an int32.
     */
     final val Ldc_I4_6 = OpCode.Ldc_I4_6

    /**
     * Pushes the integer value of 7 onto the evaluation stack as an int32.
     */
     final val Ldc_I4_7 = OpCode.Ldc_I4_7

    /**
     * Pushes the integer value of 8 onto the evaluation stack as an int32.
     */
     final val Ldc_I4_8 = OpCode.Ldc_I4_8

    /**
     * Pushes the supplied int8 value onto the evaluation stack as an int32, short form.
     */
     final val Ldc_I4_S = OpCode.Ldc_I4_S

    /**
     * Pushes a supplied value of type int32 onto the evaluation stack as an int32.
     */
     final val Ldc_I4 = OpCode.Ldc_I4

    /**
     *  Pushes a supplied value of type int64 onto the evaluation stack as an int64.
     */
     final val Ldc_I8 = OpCode.Ldc_I8

    /**
     * Pushes a supplied value of type float32 onto the evaluation stack as type F (float).
     */
     final val Ldc_R4 = OpCode.Ldc_R4

    /**
     * Pushes a supplied value of type float64 onto the evaluation stack as type F (float).
     */
     final val Ldc_R8 = OpCode.Ldc_R8

    /**
     * Copies the current topmost value on the evaluation stack, and then pushes the copy
     * onto the evaluation stack.
     */
     final val Dup = OpCode.Dup

    /**
     * Removes the value currently on top of the evaluation stack.
     */
     final val Pop = OpCode.Pop

    /**
     * Exits current method and jumps to specified method.
     */
     final val Jmp = OpCode.Jmp

    /**
     * Calls the method indicated by the passed method descriptor.
     */
     final val Call = OpCode.Call

    /**
     * constrained. prefix
     */
     final val Constrained = OpCode.Constrained

    /**
     * readonly. prefix
     */
     final val Readonly = OpCode.Readonly

    /**
     * Calls the method indicated on the evaluation stack (as a pointer to an entry point)
     * with arguments described by a calling convention.
     */
     final val Calli = OpCode.Calli

    /**
     * Returns from the current method, pushing a return value (if present) from the caller's
     * evaluation stack onto the callee's evaluation stack.
     */
     final val Ret = OpCode.Ret

    /**
     * Unconditionally transfers control to a target instruction (short form).
     */
     final val Br_S = OpCode.Br_S

    /**
     * Transfers control to a target instruction if value is false, a null reference, or zero.
     */
     final val Brfalse_S = OpCode.Brfalse_S

    /**
     * Transfers control to a target instruction (short form) if value is true, not null, or non-zero.
     */
     final val Brtrue_S = OpCode.Brtrue_S

    /**
     * Transfers control to a target instruction (short form) if two values are equal.
     */
     final val Beq_S = OpCode.Beq_S

    /**
     * Transfers control to a target instruction (short form) if the first value is greater than
     * or equal to the second value.
     */
     final val Bge_S = OpCode.Bge_S

    /**
     * Transfers control to a target instruction (short form) if the first value is greater than
     * the second value.
     */
     final val Bgt_S = OpCode.Bgt_S

    /**
     * Transfers control to a target instruction (short form) if the first value is less than
     * or equal to the second value.
     */
     final val Ble_S = OpCode.Ble_S

    /**
     * Transfers control to a target instruction (short form) if the first value is less than
     * the second value.
     */
     final val Blt_S = OpCode.Blt_S

    /**
     * Transfers control to a target instruction (short form) when two unsigned integer values
     * or unordered float values are not equal.
     */
     final val Bne_Un_S = OpCode.Bne_Un_S

    /**
     * Transfers control to a target instruction (short form) if the first value is greather
     * than the second value, when comparing unsigned integer values or unordered float values.
     */
     final val Bge_Un_S = OpCode.Bge_Un_S

    /**
     * Transfers control to a target instruction (short form) if the first value is greater than
     * the second value, when comparing unsigned integer values or unordered float values.
     */
     final val Bgt_Un_S = OpCode.Bgt_Un_S

    /**
     * Transfers control to a target instruction (short form) if the first value is less than
     * or equal to the second value, when comparing unsigned integer values or unordered float values.
     */
     final val Ble_Un_S = OpCode.Ble_Un_S

    /**
     * Transfers control to a target instruction (short form) if the first value is less than
     * the second value, when comparing unsigned integer values or unordered float values.
     */
     final val Blt_Un_S = OpCode.Blt_Un_S

    /**
     * Unconditionally transfers control to a target instruction.
     */
     final val Br = OpCode.Br

    /**
     * Transfers control to a target instruction if value is false, a null reference
     * (Nothing in Visual Basic), or zero.
     */
     final val Brfalse = OpCode.Brfalse

    /**
     * Transfers control to a target instruction if value is true, not null, or non-zero.
     */
     final val Brtrue = OpCode.Brtrue

    /**
     * Transfers control to a target instruction if two values are equal.
     */
     final val Beq = OpCode.Beq

    /**
     * Transfers control to a target instruction if the first value is greater than or
     * equal to the second value.
     */
     final val Bge = OpCode.Bge

    /**
     * Transfers control to a target instruction if the first value is greater than the second value.
     */
     final val Bgt = OpCode.Bgt

    /**
     * Transfers control to a target instruction if the first value is less than or equal
     * to the second value.
     */
     final val Ble = OpCode.Ble

    /**
     *  Transfers control to a target instruction if the first value is less than the second value.
     */
     final val Blt = OpCode.Blt

    /**
     * Transfers control to a target instruction when two unsigned integer values or
     * unordered float values are not equal.
     */
     final val Bne_Un = OpCode.Bne_Un

    /**
     * Transfers control to a target instruction if the first value is greather than
     * the second value, when comparing unsigned integer values or unordered float values.
     */
     final val Bge_Un = OpCode.Bge_Un

    /**
     * Transfers control to a target instruction if the first value is greater than the
     * second value, when comparing unsigned integer values or unordered float values.
     */
     final val Bgt_Un = OpCode.Bgt_Un

    /**
     * Transfers control to a target instruction if the first value is less than or equal to
     * the second value, when comparing unsigned integer values or unordered float values.
     */
     final val Ble_Un = OpCode.Ble_Un

    /**
     * Transfers control to a target instruction if the first value is less than the second value,
     * when comparing unsigned integer values or unordered float values.
     */
     final val Blt_Un = OpCode.Blt_Un

    /**
     * Implements a jump table.
     */
     final val Switch = OpCode.Switch

    /**
     * Loads a value of type int8 as an int32 onto the evaluation stack indirectly.
     */
     final val Ldind_I1 = OpCode.Ldind_I1

    /**
     *  Loads a value of type int16 as an int32 onto the evaluation stack indirectly.
     */
     final val Ldind_I2 = OpCode.Ldind_I2

    /**
     * Loads a value of type int32 as an int32 onto the evaluation stack indirectly.
     */
     final val Ldind_I4 = OpCode.Ldind_I4

    /**
     * Loads a value of type int64 as an int64 onto the evaluation stack indirectly.
     */
     final val Ldind_I8 = OpCode.Ldind_I8

    /**
     * Loads a value of type natural int as a natural int onto the evaluation stack indirectly.
     */
     final val Ldind_I = OpCode.Ldind_I

    /**
     *  Loads a value of type float32 as a type F (float) onto the evaluation stack indirectly.
     */
     final val Ldind_R4 = OpCode.Ldind_R4

    /**
     * Loads a value of type float64 as a type F (float) onto the evaluation stack indirectly.
     */
     final val Ldind_R8 = OpCode.Ldind_R8

    /**
     * Loads an object reference as a type O (object reference) onto the evaluation stack indirectly.
     */
     final val Ldind_Ref = OpCode.Ldind_Ref

    /**
     * Loads a value of type unsigned int8 as an int32 onto the evaluation stack indirectly.
     */
     final val Ldind_U1 = OpCode.Ldind_U1

    /**
     * Loads a value of type unsigned int16 as an int32 onto the evaluation stack indirectly.
     */
     final val Ldind_U2 = OpCode.Ldind_U2

    /**
     * Loads a value of type unsigned int32 as an int32 onto the evaluation stack indirectly.
     */
     final val Ldind_U4 = OpCode.Ldind_U4

    /**
     * Stores a object reference value at a supplied address.
     */
     final val Stind_Ref = OpCode.Stind_Ref

    /**
     * Stores a value of type int8 at a supplied address.
     */
     final val Stind_I1 = OpCode.Stind_I1

    /**
     * Stores a value of type int16 at a supplied address.
     */
     final val Stind_I2 = OpCode.Stind_I2

    /**
     * Stores a value of type int32 at a supplied address.
     */
     final val Stind_I4 = OpCode.Stind_I4

    /**
     * Stores a value of type int64 at a supplied address.
     */
     final val Stind_I8 = OpCode.Stind_I8

    /**
     * Stores a value of type float32 at a supplied address.
     */
     final val Stind_R4 = OpCode.Stind_R4

    /**
     * Stores a value of type float64 at a supplied address.
     */
     final val Stind_R8 = OpCode.Stind_R8

    /**
     * Subtracts one value from another and pushes the result onto the evaluation stack.
     */
     final val Sub = OpCode.Sub

    /**
     * Multiplies two values and pushes the result on the evaluation stack.
     */
     final val Mul = OpCode.Mul

    /**
     * Divides two values and pushes the result as a floating-point (type F) or
     * quotient (type int32) onto the evaluation stack.
     */
     final val Div = OpCode.Div

    /**
     * Divides two unsigned integer values and pushes the result (int32) onto the evaluation stack.
     */
     final val Div_Un = OpCode.Div_Un

    /**
     * Divides two values and pushes the remainder onto the evaluation stack.
     */
     final val Rem = OpCode.Rem

    /**
     * Divides two unsigned values and pushes the remainder onto the evaluation stack.
     */
     final val Rem_Un = OpCode.Rem_Un

    /**
     * Computes the bitwise AND of two values and pushes the result onto the evaluation stack.
     */
     final val And = OpCode.And

    /**
     * Compute the bitwise complement of the two integer values on top of the stack and
     * pushes the result onto the evaluation stack.
     */
     final val Or = OpCode.Or

    /**
     * Computes the bitwise XOR of the top two values on the evaluation stack,
     * pushing the result onto the evaluation stack.
     */
     final val Xor = OpCode.Xor

    /**
     * Shifts an integer value to the left (in zeroes) by a specified number of bits,
     *  pushing the result onto the evaluation stack.
     */
     final val Shl = OpCode.Shl

    /**
     * Shifts an integer value (in sign) to the right by a specified number of bits,
     * pushing the result onto the evaluation stack.
     */
     final val Shr = OpCode.Shr

    /**
     * Shifts an unsigned integer value (in zeroes) to the right by a specified number of bits,
     * pushing the result onto the evaluation stack.
     */
     final val Shr_Un = OpCode.Shr_Un

    /**
     * Negates a value and pushes the result onto the evaluation stack.
     */
     final val Neg = OpCode.Neg

    /**
     * Computes the bitwise complement of the integer value on top of the stack and pushes
     * the result onto the evaluation stack as the same type.
     */
     final val Not = OpCode.Not

    /**
     *  Converts the value on top of the evaluation stack to int8, then extends (pads) it to int32.
     */
     final val Conv_I1 = OpCode.Conv_I1

    /**
     * Converts the value on top of the evaluation stack to int16, then extends (pads) it to int32.
     */
     final val Conv_I2 = OpCode.Conv_I2

    /**
     * Converts the value on top of the evaluation stack to int32.
     */
     final val Conv_I4 = OpCode.Conv_I4

    /**
     * Converts the value on top of the evaluation stack to int64.
     */
     final val Conv_I8 = OpCode.Conv_I8

    /**
     * Converts the value on top of the evaluation stack to float32.
     */
     final val Conv_R4 = OpCode.Conv_R4

    /**
     * Converts the value on top of the evaluation stack to float64.
     */
     final val Conv_R8 = OpCode.Conv_R8

    /**
     * Converts the value on top of the evaluation stack to unsigned int32, and extends it to int32.
     */
     final val Conv_U4 = OpCode.Conv_U4

    /**
     * Converts the value on top of the evaluation stack to unsigned int64, and extends it to int64.
     */
     final val Conv_U8 = OpCode.Conv_U8

    /**
     * Calls a late-bound method on an object, pushing the return value onto the evaluation stack.
     */
     final val Callvirt = OpCode.Callvirt

    /**
     * Copies the value type located at the address of an object (type &, * or natural int)
     * to the address of the destination object (type &, * or natural int).
     */
     final val Cpobj = OpCode.Cpobj

    /**
     * Copies the value type object pointed to by an address to the top of the evaluation stack.
     */
     final val Ldobj = OpCode.Ldobj

    /**
     * Pushes a new object reference to a string literal stored in the metadata.
     */
     final val Ldstr = OpCode.Ldstr

    /**
     * Creates a new object or a new instance of a value type, pushing an object reference
     * (type O) onto the evaluation stack.
     */
     final val Newobj = OpCode.Newobj

    /**
     * Attempts to cast an object passed by reference to the specified class.
     */
     final val Castclass = OpCode.Castclass

    /**
     * Tests whether an object reference (type O) is an instance of a particular class.
     */
     final val Isinst = OpCode.Isinst

    /**
     *  Converts the unsigned integer value on top of the evaluation stack to float32.
     */
     final val Conv_R_Un = OpCode.Conv_R_Un

    /**
     * Converts the boxed representation of a value type to its unboxed form.
     */
     final val Unbox = OpCode.Unbox

    /**
     * Throws the exception object currently on the evaluation stack.
     */
     final val Throw = OpCode.Throw

    /**
     *  Finds the value of a field in the object whose reference is currently
     * on the evaluation stack.
     */
     final val Ldfld = OpCode.Ldfld

    /**
     *  Finds the address of a field in the object whose reference is currently
     * on the evaluation stack.
     */
     final val Ldflda = OpCode.Ldflda

    /**
     * Pushes the value of a static field onto the evaluation stack.
     */
     final val Ldsfld = OpCode.Ldsfld

    /**
     * Pushes the address of a static field onto the evaluation stack.
     */
     final val Ldsflda = OpCode.Ldsflda

    /**
     *  Replaces the value stored in the field of an object reference or pointer with a new value.
     */
     final val Stfld = OpCode.Stfld

    /**
     * Replaces the value of a static field with a value from the evaluation stack.
     */
     final val Stsfld = OpCode.Stsfld

    /**
     * Copies a value of a specified type from the evaluation stack into a supplied memory address.
     */
     final val Stobj = OpCode.Stobj

    /**
     * Converts the unsigned value on top of the evaluation stack to signed int8 and
     * extends it to int32, throwing OverflowException on overflow.
     */
     final val Conv_Ovf_I1_Un = OpCode.Conv_Ovf_I1_Un

    /**
     *  Converts the unsigned value on top of the evaluation stack to signed int16 and
     * extends it to int32, throwing OverflowException on overflow.
     */
     final val Conv_Ovf_I2_Un = OpCode.Conv_Ovf_I2_Un

    /**
     * Converts the unsigned value on top of the evaluation stack to signed int32,
     * throwing OverflowException on overflow.
     */
     final val Conv_Ovf_I4_Un = OpCode.Conv_Ovf_I4_Un

    /**
     * Converts the unsigned value on top of the evaluation stack to signed int64,
     * throwing OverflowException on overflow.
     */
     final val Conv_Ovf_I8_Un = OpCode.Conv_Ovf_I8_Un

    /**
     * Converts the unsigned value on top of the evaluation stack to signed natural int,
     * throwing OverflowException on overflow.
     */
     final val Conv_Ovf_I_Un = OpCode.Conv_Ovf_I_Un

    /**
     * Converts the unsigned value on top of the evaluation stack to unsigned int8 and
     * extends it to int32, throwing OverflowException on overflow.
     */
     final val Conv_Ovf_U1_Un = OpCode.Conv_Ovf_U1_Un

    /**
     * Converts the unsigned value on top of the evaluation stack to unsigned int16 and
     * extends it to int32, throwing OverflowException on overflow.
     */
     final val Conv_Ovf_U2_Un = OpCode.Conv_Ovf_U2_Un

    /**
     * Converts the unsigned value on top of the evaluation stack to unsigned int32,
     * throwing OverflowException on overflow.
     */
     final val Conv_Ovf_U4_Un = OpCode.Conv_Ovf_U4_Un

    /**
     * Converts the unsigned value on top of the evaluation stack to unsigned int64,
     * throwing OverflowException on overflow.
     */
     final val Conv_Ovf_U8_Un = OpCode.Conv_Ovf_U8_Un

    /**
     * Converts the unsigned value on top of the evaluation stack to unsigned natural int,
     * throwing OverflowException on overflow.
     */
     final val Conv_Ovf_U_Un = OpCode.Conv_Ovf_U_Un

    /**
     * Converts a value type to an object reference (type O).
     */
     final val Box = OpCode.Box

    /**
     * Pushes an object reference to a new zero-based, one-dimensional array whose elements
     * are of a specific type onto the evaluation stack.
     */
     final val Newarr = OpCode.Newarr

    /**
     * Pushes the number of elements of a zero-based, one-dimensional array
     * onto the evaluation stack.
     */
     final val Ldlen = OpCode.Ldlen

    /**
     * Loads the address of the array element at a specified array index onto
     * the top of the evaluation stack as type & (managed pointer).
     */
     final val Ldelema = OpCode.Ldelema

    /**
     * Loads the element with type natural int at a specified array index onto the top
     * of the evaluation stack as a natural int.
     */
     final val Ldelem_I = OpCode.Ldelem_I

    /**
     * Loads the element with type int8 at a specified array index onto the top of the
     * evaluation stack as an int32.
     */
     final val Ldelem_I1 = OpCode.Ldelem_I1

    /**
     * Loads the element with type int16 at a specified array index onto the top of
     * the evaluation stack as an int32.
     */
     final val Ldelem_I2 = OpCode.Ldelem_I2

    /**
     *  Loads the element with type int32 at a specified array index onto the top of the
     * evaluation stack as an int32.
     */
     final val Ldelem_I4 = OpCode.Ldelem_I4

    /**
     *  Loads the element with type int64 at a specified array index onto the top of the
     * evaluation stack as an int64.
     */
     final val Ldelem_I8 = OpCode.Ldelem_I8

    /**
     * Loads the element with type float32 at a specified array index onto the top of the
     * evaluation stack as type F (float)
     */
     final val Ldelem_R4 = OpCode.Ldelem_R4

    /**
     * Loads the element with type float64 at a specified array index onto the top of the
     * evaluation stack as type F (float) .
     */
     final val Ldelem_R8 = OpCode.Ldelem_R8

    /**
     * Loads the element containing an object reference at a specified array index onto
     * the top of the evaluation stack as type O (object reference).
     */
     final val Ldelem_Ref = OpCode.Ldelem_Ref

    /**
     * Loads the element with type unsigned int8 at a specified array index onto the top
     * of the evaluation stack as an int32.
     */
     final val Ldelem_U1 = OpCode.Ldelem_U1

    /**
     * Loads the element with type unsigned int16 at a specified array index onto the top
     * of the evaluation stack as an int32.
     */
     final val Ldelem_U2 = OpCode.Ldelem_U2

    /**
     * Loads the element with type unsigned int32 at a specified array index onto the top
     * of the evaluation stack as an int32.
     */
     final val Ldelem_U4 = OpCode.Ldelem_U4

    /**
     *  Replaces the array element at a given index with the natural int value on
     * the evaluation stack.
     */
     final val Stelem_I = OpCode.Stelem_I

    /**
     * Replaces the array element at a given index with the int8 value on the evaluation stack.
     */
     final val Stelem_I1 = OpCode.Stelem_I1

    /**
     *  Replaces the array element at a given index with the int16 value on the evaluation stack.
     */
     final val Stelem_I2 = OpCode.Stelem_I2

    /**
     *  Replaces the array element at a given index with the int32 value on the evaluation stack.
     */
     final val Stelem_I4 = OpCode.Stelem_I4

    /**
     * Replaces the array element at a given index with the int64 value on the evaluation stack.
     */
     final val Stelem_I8 = OpCode.Stelem_I8

    /**
     * Replaces the array element at a given index with the float32 value on the evaluation stack.
     */
     final val Stelem_R4 = OpCode.Stelem_R4

    /**
     * Replaces the array element at a given index with the float64 value on the evaluation stack.
     */
     final val Stelem_R8 = OpCode.Stelem_R8

    /**
     * Replaces the array element at a given index with the object ref value (type O)
     * on the evaluation stack.
     */
     final val Stelem_Ref = OpCode.Stelem_Ref

    /**
     * Converts the signed value on top of the evaluation stack to signed int8 and
     * extends it to int32, throwing OverflowException on overflow.
     */
     final val Conv_Ovf_I1 = OpCode.Conv_Ovf_I1

    /**
     * Converts the signed value on top of the evaluation stack to signed int16 and
     * extending it to int32, throwing OverflowException on overflow.
     */
     final val Conv_Ovf_I2 = OpCode.Conv_Ovf_I2

    /**
     * Converts the signed value on top of the evaluation stack to signed int32,
     * throwing OverflowException on overflow.
     */
     final val Conv_Ovf_I4 = OpCode.Conv_Ovf_I4

    /**
     * Converts the signed value on top of the evaluation stack to signed int64,
     * throwing OverflowException on overflow.
     */
     final val Conv_Ovf_I8 = OpCode.Conv_Ovf_I8

    /**
     * Converts the signed value on top of the evaluation stack to unsigned int8 and
     * extends it to int32, throwing OverflowException on overflow.
     */
     final val Conv_Ovf_U1 = OpCode.Conv_Ovf_U1

    /**
     * Converts the signed value on top of the evaluation stack to unsigned int16 and
     * extends it to int32, throwing OverflowException on overflow.
     */
     final val Conv_Ovf_U2 = OpCode.Conv_Ovf_U2

    /**
     *  Converts the signed value on top of the evaluation stack to unsigned int32,
     * throwing OverflowException on overflow.
     */
     final val Conv_Ovf_U4 = OpCode.Conv_Ovf_U4

    /**
     * Converts the signed value on top of the evaluation stack to unsigned int64,
     * throwing OverflowException on overflow.
     */
     final val Conv_Ovf_U8 = OpCode.Conv_Ovf_U8

    /**
     *  Retrieves the address (type &) embedded in a typed reference.
     */
     final val Refanyval = OpCode.Refanyval

    /**
     * Retrieves the type token embedded in a typed reference .
     */
     final val Refanytype = OpCode.Refanytype

    /**
     * Throws ArithmeticException if value is not a finite number.
     */
     final val Ckfinite = OpCode.Ckfinite

    /**
     * Pushes a typed reference to an instance of a specific type onto the evaluation stack.
     */
     final val Mkrefany = OpCode.Mkrefany

    /**
     * Converts a metadata token to its runtime representation, pushing it onto the evaluation stack.
     */
     final val Ldtoken = OpCode.Ldtoken

    /**
     * Converts the value on top of the evaluation stack to unsigned int8, and extends it to int32.
     */
     final val Conv_U1 = OpCode.Conv_U1

    /**
     * Converts the value on top of the evaluation stack to unsigned int16, and extends it to int32.
     */
     final val Conv_U2 = OpCode.Conv_U2

    /**
     * Converts the value on top of the evaluation stack to natural int.
     */
     final val Conv_I = OpCode.Conv_I

    /**
     * Converts the signed value on top of the evaluation stack to signed natural int,
     * throwing OverflowException on overflow.
     */
     final val Conv_Ovf_I = OpCode.Conv_Ovf_I

    /**
     * Converts the signed value on top of the evaluation stack to unsigned natural int,
     * throwing OverflowException on overflow.
     */
     final val Conv_Ovf_U = OpCode.Conv_Ovf_U

    /**
     * Adds two integers, performs an overflow check, and pushes the result
     * onto the evaluation stack.
     */
     final val Add_Ovf = OpCode.Add_Ovf

    /**
     *  Adds two unsigned integer values, performs an overflow check, and pushes the result
     * onto the evaluation stack.
     */
     final val Add_Ovf_Un = OpCode.Add_Ovf_Un

    /**
     * Multiplies two integer values, performs an overflow check, and pushes the result
     * onto the evaluation stack.
     */
     final val Mul_Ovf = OpCode.Mul_Ovf

    /**
     * Multiplies two unsigned integer values , performs an overflow check ,
     * and pushes the result onto the evaluation stack.
     */
     final val Mul_Ovf_Un = OpCode.Mul_Ovf_Un

    /**
     * Subtracts one integer value from another, performs an overflow check,
     * and pushes the result onto the evaluation stack.
     */
     final val Sub_Ovf = OpCode.Sub_Ovf

    /**
     * Subtracts one unsigned integer value from another, performs an overflow check,
     * and pushes the result onto the evaluation stack.
     */
     final val Sub_Ovf_Un = OpCode.Sub_Ovf_Un

    /**
     * Transfers control from the fault or finally clause of an exception block back to
     * the Common Language Infrastructure (CLI) exception handler.
     */
     final val Endfinally = OpCode.Endfinally

    /**
     * Exits a protected region of code, unconditionally tranferring control
     * to a specific target instruction.
     */
     final val Leave = OpCode.Leave

    /**
     * Exits a protected region of code, unconditionally tranferring control
     * to a target instruction (short form).
     */
     final val Leave_S = OpCode.Leave_S

    /**
     * Stores a value of type natural int at a supplied address.
     */
     final val Stind_I = OpCode.Stind_I

    /**
     *  Converts the value on top of the evaluation stack to unsigned natural int,
     * and extends it to natural int.
     */
     final val Conv_U = OpCode.Conv_U

    /**
     * Returns an unmanaged pointer to the argument list of the current method.
     */
     final val Arglist = OpCode.Arglist

    /**
     * Compares two values. If they are equal, the integer value 1 (int32) is pushed
     * onto the evaluation stack otherwise 0 (int32) is pushed onto the evaluation stack.
     */
     final val Ceq = OpCode.Ceq

    /**
     * Compares two values. If the first value is greater than the second,
     * the integer value 1 (int32) is pushed onto the evaluation stack
     * otherwise 0 (int32) is pushed onto the evaluation stack.
     */
     final val Cgt = OpCode.Cgt

    /**
     *  Compares two unsigned or unordered values. If the first value is greater than
     * the second, the integer value 1 (int32) is pushed onto the evaluation stack
     * otherwise 0 (int32) is pushed onto the evaluation stack.
     */
     final val Cgt_Un = OpCode.Cgt_Un

    /**
     * Compares two values. If the first value is less than the second,
     * the integer value 1 (int32) is pushed onto the evaluation stack
     * otherwise 0 (int32) is pushed onto the evaluation stack.
     */
     final val Clt = OpCode.Clt

    /**
     *  Compares the unsigned or unordered values value1 and value2. If value1 is
     * less than value2, then the integer value 1 (int32) is pushed onto the
     * evaluation stack otherwise 0 (int32) is pushed onto the evaluation stack.
     */
     final val Clt_Un = OpCode.Clt_Un

    /**
     * Pushes an unmanaged pointer (type natural int) to the native code implementing
     * a specific method onto the evaluation stack.
     */
     final val Ldftn = OpCode.Ldftn

    /**
     * Pushes an unmanaged pointer (type natural int) to the native code implementing
     * a particular virtual method associated with a specified object onto the evaluation stack.
     */
     final val Ldvirtftn = OpCode.Ldvirtftn

    /**
     * Loads an argument (referenced by a specified index value) onto the stack.
     */
     final val Ldarg = OpCode.Ldarg

    /**
     * Load an argument address onto the evaluation stack.
     */
     final val Ldarga = OpCode.Ldarga

    /**
     * Loads the local variable at a specific index onto the evaluation stack.
     */
     final val Ldloc = OpCode.Ldloc

    /**
     *  Loads the address of the local variable at a specific index onto the evaluation stack.
     */
     final val Ldloca = OpCode.Ldloca

    /**
     *  Stores the value on top of the evaluation stack in the argument slot at a specified index.
     */
     final val Starg = OpCode.Starg

    /**
     * Pops the current value from the top of the evaluation stack and stores it in a
     * the local variable list at a specified index.
     */
     final val Stloc = OpCode.Stloc

    /**
     * Allocates a certain number of bytes from the local dynamic memory pool and pushes the
     * address (a transient pointer, type *) of the first allocated Byte onto the evaluation stack.
     */
     final val Localloc = OpCode.Localloc

    /**
     * Transfers control from the filter clause of an exception back to the
     * Common Language Infrastructure (CLI) exception handler.
     */
     final val Endfilter = OpCode.Endfilter

    /**
     * Indicates that an address currently atop the evaluation stack might not be aligned
     * to the natural size of the immediately following ldind, stind, ldfld, stfld, ldobj,
     * stobj, initblk, or cpblk instruction.
     */
     final val Unaligned = OpCode.Unaligned

    /**
     * Specifies that an address currently atop the evaluation stack might be volatile,
     * and the results of reading that location cannot be cached or that multiple stores
     * to that location cannot be suppressed.
     */
     final val Volatile = OpCode.Volatile

    /**
     * Performs a postfixed method call instruction such that the current method's stack
     * frame is removed before the actual call instruction is executed.
     */
     final val Tailcall = OpCode.Tailcall

    /**
     * Initializes all the fields of the object at a specific address to a null reference
     * or a 0 of the appropriate primitive type.
     */
     final val Initobj = OpCode.Initobj

    /**
     * Copies a specified number bytes from a source address to a destination address .
     */
     final val Cpblk = OpCode.Cpblk

    /**
     * Initializes a specified block of memory at a specific address to a given size
     * and initial value.
     */
     final val Initblk = OpCode.Initblk

    /**
     * Rethrows the current exception.
     */
     final val Rethrow = OpCode.Rethrow

    /**
     * Pushes the size, in bytes, of a supplied value type onto the evaluation stack.
     */
     final val Sizeof = OpCode.Sizeof

    //##########################################################################
}
