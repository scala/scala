/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2012 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.*;

import ch.epfl.lamp.util.ByteArray;

/**
 * List of instructions, to which Java byte-code instructions can be added.
 *
 * @author Michel Schinz, Thomas Friedli
 * @version 1.0
 */

public class JCode {
    protected boolean frozen = false;

    public static int MAX_CODE_SIZE = 65535;

    protected final FJBGContext context;
    protected final JMethod owner;

    protected final ByteArray codeArray;

    protected final LinkedList/*<ExceptionHandler>*/ exceptionHandlers =
        new LinkedList();

    protected final JConstantPool pool;

    protected final ArrayList/*<OffsetToPatch>*/ offsetToPatch =
        new ArrayList();

    protected static int UNKNOWN_STACK_SIZE = Integer.MIN_VALUE;
    protected int maxStackSize = UNKNOWN_STACK_SIZE;
    protected int[] stackProduction = null;
    protected int[] stackSizes;

    protected JCode(FJBGContext context, JClass clazz, JMethod owner) {
        this.context = context;
        this.pool = clazz.getConstantPool();
        this.owner = owner;
        this.codeArray = new ByteArray();
    }

    protected JCode(FJBGContext context,
                    JClass clazz,
                    JMethod owner,
                    DataInputStream stream)
        throws IOException {
        this.context = context;
        this.pool = clazz.getConstantPool();
        this.owner = owner;
        owner.setCode(this);
        int size = stream.readInt();
        if (size > MAX_CODE_SIZE) // section 4.10
            throw new Error("code size must be less than " + MAX_CODE_SIZE + ": " + size);
        this.codeArray = new ByteArray(stream, size);
    }

    /**
     * Gets the program counter, which is defined as the address of the
     * next instruction.
     * @return The int representing the value of the program counter
     */
    public int getPC() {
        return codeArray.getSize();
    }

    /**
     * Gets the size of the code
     * @return The number of bytes of the code
     */
    public int getSize() {
        return codeArray.getSize();
    }

    /**
     * Gets the method to which the code belongs
     * @return The method to which the code belongs
     */
    public JMethod getOwner() {
        return owner;
    }

    // Stack size
    public int getMaxStackSize() {
        if (maxStackSize == UNKNOWN_STACK_SIZE)
            maxStackSize = computeMaxStackSize();
        return maxStackSize;
    }

    // Freezing
    //////////////////////////////////////////////////////////////////////

    public static class CodeSizeTooBigException extends OffsetTooBigException {
        public int codeSize;

        public CodeSizeTooBigException(int size) {
          codeSize = size;
        }
    }

    public void freeze() throws OffsetTooBigException {
        assert !frozen;

        if (getSize() > MAX_CODE_SIZE) throw new CodeSizeTooBigException(getSize());

        patchAllOffset();
        codeArray.freeze();
        frozen = true;
    }

    // Attributes
    //////////////////////////////////////////////////////////////////////

    protected final LinkedList/*<JAttribute>*/ attributes = new LinkedList();

    public void addAttribute(JAttribute attr) {
        attributes.add(attr);
    }

    public List/*<JAttribute>*/ getAttributes() {
        return attributes;
    }

    // Emitting code
    //////////////////////////////////////////////////////////////////////

    public void emit(JOpcode opcode) {
        setStackProduction(getPC(), opcode);
        codeArray.addU1(opcode.code);
    }

    public void emitNOP() { emit(JOpcode.NOP); }

    // Constant loading.
    public void emitACONST_NULL() { emit(JOpcode.ACONST_NULL); }
    public void emitICONST_M1() { emit(JOpcode.ICONST_M1); }
    public void emitICONST_0() { emit(JOpcode.ICONST_0); }
    public void emitICONST_1() { emit(JOpcode.ICONST_1); }
    public void emitICONST_2() { emit(JOpcode.ICONST_2); }
    public void emitICONST_3() { emit(JOpcode.ICONST_3); }
    public void emitICONST_4() { emit(JOpcode.ICONST_4); }
    public void emitICONST_5() { emit(JOpcode.ICONST_5); }
    public void emitLCONST_0() { emit(JOpcode.LCONST_0); }
    public void emitLCONST_1() { emit(JOpcode.LCONST_1); }
    public void emitFCONST_0() { emit(JOpcode.FCONST_0); }
    public void emitFCONST_1() { emit(JOpcode.FCONST_1); }
    public void emitFCONST_2() { emit(JOpcode.FCONST_2); }
    public void emitDCONST_0() { emit(JOpcode.DCONST_0); }
    public void emitDCONST_1() { emit(JOpcode.DCONST_1); }

    public void emitBIPUSH(int b) { emitU1(JOpcode.BIPUSH, b); }
    public void emitSIPUSH(int s) { emitU2(JOpcode.SIPUSH, s); }
    public void emitLDC(int value) {
        emitU1(JOpcode.LDC, pool.addInteger(value));
    }
    public void emitLDC(float value) {
        emitU1(JOpcode.LDC, pool.addFloat(value));
    }
    public void emitLDC(String value) {
        emitU1(JOpcode.LDC, pool.addString(value));
    }
    public void emitLDC_W(int value) {
        emitU1(JOpcode.LDC_W, pool.addInteger(value));
    }
    public void emitLDC_W(float value) {
        emitU1(JOpcode.LDC_W, pool.addFloat(value));
    }
    public void emitLDC_W(String value) {
        emitU1(JOpcode.LDC_W, pool.addString(value));
    }
    public void emitLDC2_W(long value) {
        emitU2(JOpcode.LDC2_W, pool.addLong(value));
    }
    public void emitLDC2_W(double value) {
        emitU2(JOpcode.LDC2_W, pool.addDouble(value));
    }

    // Loading variables.
    public void emitILOAD(int index) { emitU1(JOpcode.ILOAD, index); }
    public void emitLLOAD(int index) { emitU1(JOpcode.LLOAD, index); }
    public void emitFLOAD(int index) { emitU1(JOpcode.FLOAD, index); }
    public void emitDLOAD(int index) { emitU1(JOpcode.DLOAD, index); }
    public void emitALOAD(int index) { emitU1(JOpcode.ALOAD, index); }

    public void emitILOAD_0() { emit(JOpcode.ILOAD_0); }
    public void emitILOAD_1() { emit(JOpcode.ILOAD_1); }
    public void emitILOAD_2() { emit(JOpcode.ILOAD_2); }
    public void emitILOAD_3() { emit(JOpcode.ILOAD_3); }
    public void emitLLOAD_0() { emit(JOpcode.LLOAD_0); }
    public void emitLLOAD_1() { emit(JOpcode.LLOAD_1); }
    public void emitLLOAD_2() { emit(JOpcode.LLOAD_2); }
    public void emitLLOAD_3() { emit(JOpcode.LLOAD_3); }
    public void emitFLOAD_0() { emit(JOpcode.FLOAD_0); }
    public void emitFLOAD_1() { emit(JOpcode.FLOAD_1); }
    public void emitFLOAD_2() { emit(JOpcode.FLOAD_2); }
    public void emitFLOAD_3() { emit(JOpcode.FLOAD_3); }
    public void emitDLOAD_0() { emit(JOpcode.DLOAD_0); }
    public void emitDLOAD_1() { emit(JOpcode.DLOAD_1); }
    public void emitDLOAD_2() { emit(JOpcode.DLOAD_2); }
    public void emitDLOAD_3() { emit(JOpcode.DLOAD_3); }
    public void emitALOAD_0() { emit(JOpcode.ALOAD_0); }
    public void emitALOAD_1() { emit(JOpcode.ALOAD_1); }
    public void emitALOAD_2() { emit(JOpcode.ALOAD_2); }
    public void emitALOAD_3() { emit(JOpcode.ALOAD_3); }

    public void emitIALOAD() { emit(JOpcode.IALOAD); }
    public void emitLALOAD() { emit(JOpcode.LALOAD); }
    public void emitFALOAD() { emit(JOpcode.FALOAD); }
    public void emitDALOAD() { emit(JOpcode.DALOAD); }
    public void emitAALOAD() { emit(JOpcode.AALOAD); }
    public void emitBALOAD() { emit(JOpcode.BALOAD); }
    public void emitCALOAD() { emit(JOpcode.CALOAD); }
    public void emitSALOAD() { emit(JOpcode.SALOAD); }

    // Storing variables.
    public void emitISTORE(int index) { emitU1(JOpcode.ISTORE, index); }
    public void emitLSTORE(int index) { emitU1(JOpcode.LSTORE, index); }
    public void emitFSTORE(int index) { emitU1(JOpcode.FSTORE, index); }
    public void emitDSTORE(int index) { emitU1(JOpcode.DSTORE, index); }
    public void emitASTORE(int index) { emitU1(JOpcode.ASTORE, index); }

    public void emitISTORE_0() { emit(JOpcode.ISTORE_0); }
    public void emitISTORE_1() { emit(JOpcode.ISTORE_1); }
    public void emitISTORE_2() { emit(JOpcode.ISTORE_2); }
    public void emitISTORE_3() { emit(JOpcode.ISTORE_3); }
    public void emitLSTORE_0() { emit(JOpcode.LSTORE_0); }
    public void emitLSTORE_1() { emit(JOpcode.LSTORE_1); }
    public void emitLSTORE_2() { emit(JOpcode.LSTORE_2); }
    public void emitLSTORE_3() { emit(JOpcode.LSTORE_3); }
    public void emitFSTORE_0() { emit(JOpcode.FSTORE_0); }
    public void emitFSTORE_1() { emit(JOpcode.FSTORE_1); }
    public void emitFSTORE_2() { emit(JOpcode.FSTORE_2); }
    public void emitFSTORE_3() { emit(JOpcode.FSTORE_3); }
    public void emitDSTORE_0() { emit(JOpcode.DSTORE_0); }
    public void emitDSTORE_1() { emit(JOpcode.DSTORE_1); }
    public void emitDSTORE_2() { emit(JOpcode.DSTORE_2); }
    public void emitDSTORE_3() { emit(JOpcode.DSTORE_3); }
    public void emitASTORE_0() { emit(JOpcode.ASTORE_0); }
    public void emitASTORE_1() { emit(JOpcode.ASTORE_1); }
    public void emitASTORE_2() { emit(JOpcode.ASTORE_2); }
    public void emitASTORE_3() { emit(JOpcode.ASTORE_3); }

    public void emitIASTORE() { emit(JOpcode.IASTORE); }
    public void emitLASTORE() { emit(JOpcode.LASTORE); }
    public void emitFASTORE() { emit(JOpcode.FASTORE); }
    public void emitDASTORE() { emit(JOpcode.DASTORE); }
    public void emitAASTORE() { emit(JOpcode.AASTORE); }
    public void emitBASTORE() { emit(JOpcode.BASTORE); }
    public void emitCASTORE() { emit(JOpcode.CASTORE); }
    public void emitSASTORE() { emit(JOpcode.SASTORE); }

    // Stack manipulation.
    public void emitPOP() { emit(JOpcode.POP); }
    public void emitPOP2() { emit(JOpcode.POP2); }
    public void emitDUP() { emit(JOpcode.DUP); }
    public void emitDUP_X1() { emit(JOpcode.DUP_X1); }
    public void emitDUP_X2() { emit(JOpcode.DUP_X2); }
    public void emitDUP2() { emit(JOpcode.DUP2); }
    public void emitDUP2_X1() { emit(JOpcode.DUP2_X1); }
    public void emitDUP2_X2() { emit(JOpcode.DUP2_X2); }
    public void emitSWAP() { emit(JOpcode.SWAP); }

    // Artithmetic and logic operations.
    public void emitIADD() { emit(JOpcode.IADD); }
    public void emitLADD() { emit(JOpcode.LADD); }
    public void emitFADD() { emit(JOpcode.FADD); }
    public void emitDADD() { emit(JOpcode.DADD); }

    public void emitISUB() { emit(JOpcode.ISUB); }
    public void emitLSUB() { emit(JOpcode.LSUB); }
    public void emitFSUB() { emit(JOpcode.FSUB); }
    public void emitDSUB() { emit(JOpcode.DSUB); }

    public void emitIMUL() { emit(JOpcode.IMUL); }
    public void emitLMUL() { emit(JOpcode.LMUL); }
    public void emitFMUL() { emit(JOpcode.FMUL); }
    public void emitDMUL() { emit(JOpcode.DMUL); }

    public void emitIDIV() { emit(JOpcode.IDIV); }
    public void emitLDIV() { emit(JOpcode.LDIV); }
    public void emitFDIV() { emit(JOpcode.FDIV); }
    public void emitDDIV() { emit(JOpcode.DDIV); }

    public void emitIREM() { emit(JOpcode.IREM); }
    public void emitLREM() { emit(JOpcode.LREM); }
    public void emitFREM() { emit(JOpcode.FREM); }
    public void emitDREM() { emit(JOpcode.DREM); }

    public void emitINEG() { emit(JOpcode.INEG); }
    public void emitLNEG() { emit(JOpcode.LNEG); }
    public void emitFNEG() { emit(JOpcode.FNEG); }
    public void emitDNEG() { emit(JOpcode.DNEG); }

    public void emitISHL() { emit(JOpcode.ISHL); }
    public void emitLSHL() { emit(JOpcode.LSHL); }

    public void emitISHR() { emit(JOpcode.ISHR); }
    public void emitLSHR() { emit(JOpcode.LSHR); }

    public void emitIUSHR() { emit(JOpcode.IUSHR); }
    public void emitLUSHR() { emit(JOpcode.LUSHR); }

    public void emitIAND() { emit(JOpcode.IAND); }
    public void emitLAND() { emit(JOpcode.LAND); }

    public void emitIOR() { emit(JOpcode.IOR); }
    public void emitLOR() { emit(JOpcode.LOR); }

    public void emitIXOR() { emit(JOpcode.IXOR); }
    public void emitLXOR() { emit(JOpcode.LXOR); }

    public void emitIINC(int index, int increment) {
        emitU1U1(JOpcode.IINC, index, increment);
    }

    // (Numeric) type conversions.
    public void emitI2L() { emit(JOpcode.I2L); }
    public void emitI2F() { emit(JOpcode.I2F); }
    public void emitI2D() { emit(JOpcode.I2D); }
    public void emitL2I() { emit(JOpcode.L2I); }
    public void emitL2F() { emit(JOpcode.L2F); }
    public void emitL2D() { emit(JOpcode.L2D); }
    public void emitF2I() { emit(JOpcode.F2I); }
    public void emitF2L() { emit(JOpcode.F2L); }
    public void emitF2D() { emit(JOpcode.F2D); }
    public void emitD2I() { emit(JOpcode.D2I); }
    public void emitD2L() { emit(JOpcode.D2L); }
    public void emitD2F() { emit(JOpcode.D2F); }
    public void emitI2B() { emit(JOpcode.I2B); }
    public void emitI2C() { emit(JOpcode.I2C); }
    public void emitI2S() { emit(JOpcode.I2S); }

    // Comparisons and tests.
    public void emitLCMP() { emit(JOpcode.LCMP); }
    public void emitFCMPL() { emit(JOpcode.FCMPL); }
    public void emitFCMPG() { emit(JOpcode.FCMPG); }
    public void emitDCMPL() { emit(JOpcode.DCMPL); }
    public void emitDCMPG() { emit(JOpcode.DCMPG); }

    protected void emitGenericIF(JOpcode opcode, Label label)
        throws OffsetTooBigException {
        emitU2(opcode, label.getOffset16(getPC() + 1, getPC()));
    }

    public void emitIFEQ(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IFEQ, label);
    }
    public void emitIFEQ(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IFEQ, targetPC - getPC());
    }
    public void emitIFEQ() {
        emitU2(JOpcode.IFEQ, 0);
    }

    public void emitIFNE(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IFNE, label);
    }
    public void emitIFNE(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IFNE, targetPC - getPC());
    }
    public void emitIFNE() {
        emitU2(JOpcode.IFNE, 0);
    }

    public void emitIFLT(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IFLT, label);
    }
    public void emitIFLT(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IFLT, targetPC - getPC());
    }
    public void emitIFLT() {
        emitU2(JOpcode.IFLT, 0);
    }

    public void emitIFGE(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IFGE, label);
    }
    public void emitIFGE(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IFGE, targetPC - getPC());
    }
    public void emitIFGE() {
        emitU2(JOpcode.IFGE, 0);
    }

    public void emitIFGT(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IFGT, label);
    }
    public void emitIFGT(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IFGT, targetPC - getPC());
    }
    public void emitIFGT() {
        emitU2(JOpcode.IFGT, 0);
    }

    public void emitIFLE(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IFLE, label);
    }
    public void emitIFLE(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IFLE, targetPC - getPC());
    }
    public void emitIFLE() {
        emitU2(JOpcode.IFLE, 0);
    }

    public void emitIF_ICMPEQ(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IF_ICMPEQ, label);
    }
    public void emitIF_ICMPEQ(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IF_ICMPEQ, targetPC - getPC());
    }
    public void emitIF_ICMPEQ() {
        emitU2(JOpcode.IF_ICMPEQ, 0);
    }

    public void emitIF_ICMPNE(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IF_ICMPNE, label);
    }
    public void emitIF_ICMPNE(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IF_ICMPNE, targetPC - getPC());
    }
    public void emitIF_ICMPNE() {
        emitU2(JOpcode.IF_ICMPNE, 0);
    }

    public void emitIF_ICMPLT(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IF_ICMPLT, label);
    }
    public void emitIF_ICMPLT(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IF_ICMPLT, targetPC - getPC());
    }
    public void emitIF_ICMPLT() {
        emitU2(JOpcode.IF_ICMPLT, 0);
    }

    public void emitIF_ICMPGE(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IF_ICMPGE, label);
    }
    public void emitIF_ICMPGE(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IF_ICMPGE, targetPC - getPC());
    }
    public void emitIF_ICMPGE() {
        emitU2(JOpcode.IF_ICMPGE, 0);
    }

    public void emitIF_ICMPGT(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IF_ICMPGT, label);
    }
    public void emitIF_ICMPGT(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IF_ICMPGT, targetPC - getPC());
    }
    public void emitIF_ICMPGT() {
        emitU2(JOpcode.IF_ICMPGT, 0);
    }

    public void emitIF_ICMPLE(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IF_ICMPLE, label);
    }
    public void emitIF_ICMPLE(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IF_ICMPLE, targetPC - getPC());
    }
    public void emitIF_ICMPLE() {
        emitU2(JOpcode.IF_ICMPLE, 0);
    }

    public void emitIF_ACMPEQ(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IF_ACMPEQ, label);
    }
    public void emitIF_ACMPEQ(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IF_ACMPEQ, targetPC - getPC());
    }
    public void emitIF_ACMPEQ() {
        emitU2(JOpcode.IF_ACMPEQ, 0);
    }

    public void emitIF_ACMPNE(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IF_ACMPNE, label);
    }
    public void emitIF_ACMPNE(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IF_ACMPNE, targetPC - getPC());
    }
    public void emitIF_ACMPNE() {
        emitU2(JOpcode.IF_ACMPNE, 0);
    }

    public void emitIFNULL(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IFNULL, label);
    }
    public void emitIFNULL(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IFNULL, targetPC - getPC());
    }
    public void emitIFNULL() {
        emitU2(JOpcode.IFNULL, 0);
    }

    public void emitIFNONNULL(Label label) throws OffsetTooBigException {
        emitGenericIF(JOpcode.IFNONNULL, label);
    }
    public void emitIFNONNULL(int targetPC) throws OffsetTooBigException {
        emitU2(JOpcode.IFNONNULL, targetPC - getPC());
    }
    public void emitIFNONNULL() {
        emitU2(JOpcode.IFNONNULL, 0);
    }

    public void emitGOTO(Label label) throws OffsetTooBigException {
        emitU2(JOpcode.GOTO, label.getOffset16(getPC() + 1, getPC()));
    }
    public void emitGOTO(int targetPC) throws OffsetTooBigException {
        int offset = targetPC - getPC();
        checkOffset16(offset);
        emitU2(JOpcode.GOTO, offset);
    }
    public void emitGOTO() {
        emitU2(JOpcode.GOTO, 0);
    }

    public void emitGOTO_W(Label label) {
        emitU4(JOpcode.GOTO_W, label.getOffset32(getPC() + 1, getPC()));
    }
    public void emitGOTO_W(int targetPC) {
        emitU4(JOpcode.GOTO_W, targetPC - getPC());
    }
    public void emitGOTO_W() {
        emitU4(JOpcode.GOTO_W, 0);
    }

    public void emitJSR(Label label) throws OffsetTooBigException {
        emitU2(JOpcode.JSR, label.getOffset16(getPC() + 1, getPC()));
    }
    public void emitJSR(int targetPC) {
        emitU2(JOpcode.JSR, targetPC - getPC());
    }
    public void emitJSR() {
        emitU2(JOpcode.JSR, 0);
    }

    public void emitJSR_W(Label label) {
        emitU4(JOpcode.JSR_W, label.getOffset32(getPC() + 1, getPC()));
    }
    public void emitJSR_W(int targetPC) {
        emitU4(JOpcode.JSR_W, targetPC - getPC());
    }
    public void emitJSR_W() {
        emitU4(JOpcode.JSR_W, 0);
    }

    /*
    public void emitRET(Label label) throws OffsetTooBigException {
        emitU2(JOpcode.RET, label.getOffset16(getPC() + 1, getPC()));
    }
    public void emitRET(int targetPC) {
        emitU1(JOpcode.RET, targetPC);
    }
    public void emitRET() {
        emitU1(JOpcode.RET, 0);
    }
    */

    public void emitRET(int index) {
        emitU1(JOpcode.RET, index);
    }

    public void emitRET(JLocalVariable var) {
        emitRET(var.getIndex());
    }

    public void emitTABLESWITCH(int[] keys,
                     Label[] branches,
                     Label defaultBranch) {
        assert keys.length == branches.length;

        int low = keys[0], high = keys[keys.length - 1];
        int instrPC = getPC();

        setStackProduction(instrPC, JOpcode.TABLESWITCH);
        codeArray.addU1(JOpcode.cTABLESWITCH);
        while (getPC() % 4 != 0) codeArray.addU1(0);

        codeArray.addU4(defaultBranch.getOffset32(getPC(), instrPC));
        codeArray.addU4(low);
        codeArray.addU4(high);
        for (int i = 0; i < branches.length; i++) {
            assert keys[i] == low + i;
            codeArray.addU4(branches[i].getOffset32(getPC(), instrPC));
        }
    }

    public void emitLOOKUPSWITCH(int[] keys,
                     Label[] branches,
                     Label defaultBranch) {
        assert keys.length == branches.length;

        int instrPC = getPC();
        setStackProduction(getPC(), JOpcode.LOOKUPSWITCH);
        codeArray.addU1(JOpcode.cLOOKUPSWITCH);
        while (getPC() % 4 != 0) codeArray.addU1(0);

        codeArray.addU4(defaultBranch.getOffset32(getPC(), instrPC));
        codeArray.addU4(branches.length);
        for (int i = 0; i < branches.length; i++) {
            codeArray.addU4(keys[i]);
            codeArray.addU4(branches[i].getOffset32(getPC(), instrPC));
        }
    }

    public void emitIRETURN() { emit(JOpcode.IRETURN); }
    public void emitLRETURN() { emit(JOpcode.LRETURN); }
    public void emitFRETURN() { emit(JOpcode.FRETURN); }
    public void emitDRETURN() { emit(JOpcode.DRETURN); }
    public void emitARETURN() { emit(JOpcode.ARETURN); }
    public void emitRETURN() { emit(JOpcode.RETURN); }

    // Field access
    public void emitGETSTATIC(String className, String name, JType type) {
        setStackProduction(getPC(), type.getSize());
        int index = pool.addFieldRef(className, name, type.getSignature());
        emitU2(JOpcode.GETSTATIC, index);
    }
    public void emitPUTSTATIC(String className, String name, JType type) {
        setStackProduction(getPC(), -type.getSize());
        int index = pool.addFieldRef(className, name, type.getSignature());
        emitU2(JOpcode.PUTSTATIC, index);
    }
    public void emitGETFIELD(String className, String name, JType type) {
        setStackProduction(getPC(), type.getSize() - 1);
        int index = pool.addFieldRef(className, name, type.getSignature());
        emitU2(JOpcode.GETFIELD, index);
    }
    public void emitPUTFIELD(String className, String name, JType type) {
        setStackProduction(getPC(), -(type.getSize() + 1));
        int index = pool.addFieldRef(className, name, type.getSignature());
        emitU2(JOpcode.PUTFIELD, index);
    }

    // Method invocation
    public void emitINVOKEVIRTUAL(String className,
                                  String name,
                                  JMethodType type) {
        setStackProduction(getPC(), type.getProducedStack() - 1);
        int index =
            pool.addClassMethodRef(className, name, type.getSignature());
        emitU2(JOpcode.INVOKEVIRTUAL, index);
    }
    public void emitINVOKESPECIAL(String className,
                                  String name,
                                  JMethodType type) {
        setStackProduction(getPC(), type.getProducedStack() - 1);
        int index =
            pool.addClassMethodRef(className, name, type.getSignature());
        emitU2(JOpcode.INVOKESPECIAL, index);
    }
    public void emitINVOKESTATIC(String className,
                                 String name,
                                 JMethodType type) {
        setStackProduction(getPC(), type.getProducedStack());
        int index =
            pool.addClassMethodRef(className, name, type.getSignature());
        emitU2(JOpcode.INVOKESTATIC, index);
    }
    public void emitINVOKEINTERFACE(String className,
                                    String name,
                                    JMethodType type) {
        setStackProduction(getPC(), type.getProducedStack() - 1);
        int index =
            pool.addInterfaceMethodRef(className, name, type.getSignature());
        emitU2U1U1(JOpcode.INVOKEINTERFACE, index, type.getArgsSize() + 1, 0);
    }

    // Object creation
    public void emitNEW(String className) {
        emitU2(JOpcode.NEW, pool.addClass(className));
    }
    public void emitNEWARRAY(JType elemType) {
        emitU1(JOpcode.NEWARRAY, elemType.getTag());
    }
    public void emitANEWARRAY(JReferenceType elemType) {
        emitU2(JOpcode.ANEWARRAY, pool.addDescriptor(elemType));
    }
    public void emitMULTIANEWARRAY(JReferenceType elemType, int dimensions) {
        setStackProduction(getPC(), -dimensions + 1);
        emitU2U1(JOpcode.MULTIANEWARRAY,
                 pool.addDescriptor(elemType),
                 dimensions);
    }
    public void emitARRAYLENGTH() { emit(JOpcode.ARRAYLENGTH); }

    // Exception throwing
    public void emitATHROW() { emit(JOpcode.ATHROW); }

    // Dynamic typing
    public void emitCHECKCAST(JReferenceType type) {
        emitU2(JOpcode.CHECKCAST, pool.addDescriptor(type));
    }
    public void emitINSTANCEOF(JReferenceType type) {
        emitU2(JOpcode.INSTANCEOF, pool.addDescriptor(type));
    }

    // Monitors
    public void emitMONITORENTER() { emit(JOpcode.MONITORENTER); }
    public void emitMONITOREXIT() { emit(JOpcode.MONITOREXIT); }

    // Wide variants
    // FIXME setStackProd. will here raise an exception
    public void emitWIDE(JOpcode opcode, int index) {
        assert (opcode.code == JOpcode.cILOAD)
            || (opcode.code == JOpcode.cLLOAD)
            || (opcode.code == JOpcode.cFLOAD)
            || (opcode.code == JOpcode.cDLOAD)
            || (opcode.code == JOpcode.cALOAD)
            || (opcode.code == JOpcode.cISTORE)
            || (opcode.code == JOpcode.cLSTORE)
            || (opcode.code == JOpcode.cFSTORE)
            || (opcode.code == JOpcode.cDSTORE)
            || (opcode.code == JOpcode.cASTORE)
            || (opcode.code == JOpcode.cRET)
            : "invalide opcode for WIDE: " + opcode;

        setStackProduction(getPC(), opcode);
        codeArray.addU1(JOpcode.WIDE.code);
        codeArray.addU1(opcode.code);
        codeArray.addU2(index);
    }
    public void emitWIDE(JOpcode opcode, int index, int constant) {
        assert opcode.code == JOpcode.cIINC
            : "invalid opcode for WIDE: " + opcode;

        setStackProduction(getPC(), opcode);
        codeArray.addU1(JOpcode.cWIDE);
        codeArray.addU1(opcode.code);
        codeArray.addU2(index);
        codeArray.addU2(constant);
    }

    protected void emitU1(JOpcode opcode, int i1) {
        setStackProduction(getPC(), opcode);
        codeArray.addU1(opcode.code);
        codeArray.addU1(i1);
    }

    protected void emitU1U1(JOpcode opcode, int i1, int i2) {
        setStackProduction(getPC(), opcode);
        codeArray.addU1(opcode.code);
        codeArray.addU1(i1);
        codeArray.addU1(i2);
    }

    protected void emitU2(JOpcode opcode, int i1) {
        setStackProduction(getPC(), opcode);
        codeArray.addU1(opcode.code);
        codeArray.addU2(i1);
    }

    protected void emitU2U1(JOpcode opcode, int i1, int i2) {
        setStackProduction(getPC(), opcode);
        codeArray.addU1(opcode.code);
        codeArray.addU2(i1);
        codeArray.addU1(i2);
    }

    protected void emitU2U1U1(JOpcode opcode, int i1, int i2, int i3) {
        setStackProduction(getPC(), opcode);
        codeArray.addU1(opcode.code);
        codeArray.addU2(i1);
        codeArray.addU1(i2);
        codeArray.addU1(i3);
    }

    protected void emitU4(JOpcode opcode, int i1) {
        setStackProduction(getPC(), opcode);
        codeArray.addU1(opcode.code);
        codeArray.addU4(i1);
    }

    protected int getU1(int sourcePos) {
        return codeArray.getU1(sourcePos);
    }

    protected int getU2(int sourcePos) {
        return codeArray.getU2(sourcePos);
    }

    protected int getU4(int sourcePos) {
        return codeArray.getU4(sourcePos);
    }

    protected int getS1(int sourcePos) {
        return codeArray.getS1(sourcePos);
    }

    protected int getS2(int sourcePos) {
        return codeArray.getS2(sourcePos);
    }

    protected int getS4(int sourcePos) {
        return codeArray.getS4(sourcePos);
    }

    // Stack size computation
    //////////////////////////////////////////////////////////////////////

    protected int getStackProduction(int pc) {
        if (stackProduction == null || pc >= stackProduction.length)
            return UNKNOWN_STACK_SIZE;
        else
            return stackProduction[pc];
    }

    protected void setStackProduction(int pc, int production) {
        if (stackProduction == null) {
            stackProduction = new int[256];
            Arrays.fill(stackProduction, UNKNOWN_STACK_SIZE);
        } else {
            while (pc >= stackProduction.length) {
                    int[] newStackProduction = new int[stackProduction.length * 2];
                    System.arraycopy(stackProduction, 0,
                                     newStackProduction, 0,
                                     stackProduction.length);
                    Arrays.fill(newStackProduction,
                                stackProduction.length,
                                newStackProduction.length,
                                UNKNOWN_STACK_SIZE);
                    stackProduction = newStackProduction;
                }
        }
        stackProduction[pc] = production;
    }

    protected void setStackProduction(int pc, JOpcode opcode) {
        // TODO we should instead check whether the opcode has known
        // stack consumption/production.
        if (getStackProduction(pc) == UNKNOWN_STACK_SIZE)
//                && opcode.hasKnownProducedDataSize()
//                && opcode.hasKnownConsumedDataSize())
            setStackProduction(pc,
                               opcode.getProducedDataSize()
                               - opcode.getConsumedDataSize());
    }

    protected int computeMaxStackSize() {
        if (stackSizes == null) {
            stackSizes = new int[getSize()];
            Arrays.fill(stackSizes, UNKNOWN_STACK_SIZE);
            stackSizes[0] = 0;
        }
        int size = computeMaxStackSize(0, 0, 0);

        // compute stack sizes for exception handlers too
        ExceptionHandler exh = null;
        for (Iterator it = exceptionHandlers.iterator();
             it.hasNext();) {
            exh = (ExceptionHandler)it.next();
            int exhSize = computeMaxStackSize(exh.getHandlerPC(), 1, 1);
            if (size < exhSize)
                size = exhSize;
        }

        return size;
    }

    protected int computeMaxStackSize(int pc, int stackSize, int maxStackSize) {
        JCodeIterator iterator = new JCodeIterator(this, pc);
        for (;;) {
            int successors = iterator.getSuccessorCount();
            if (successors == 0)
                return maxStackSize;
            else {
                assert stackProduction[iterator.getPC()] != UNKNOWN_STACK_SIZE
                    : "unknown stack production, pc=" + iterator.getPC()
                    + " in method " + owner.getName();
                stackSize += stackProduction[iterator.getPC()];
                if (stackSize > maxStackSize)
                    maxStackSize = stackSize;
                int nextPC = -1;
                for (int i = 0; i < successors; ++i) {
                    int succPC = iterator.getSuccessorPC(i);
                    assert succPC >= 0 && succPC < stackSizes.length
                        : iterator.getPC() + ": invalid pc: " + succPC
                        + " op: " + iterator.getOpcode();
                    if (stackSizes[succPC] == UNKNOWN_STACK_SIZE) {
                        stackSizes[succPC] = stackSize;
                        if (nextPC == -1)
                            nextPC = succPC;
                        else
                            maxStackSize = computeMaxStackSize(succPC,
                                                               stackSize,
                                                               maxStackSize);
                    }
                }
                if (nextPC == -1)
                    return maxStackSize;
                else
                    iterator.moveTo(nextPC);
            }
        }
    }

    // Labels
    //////////////////////////////////////////////////////////////////////

    public static class OffsetTooBigException extends Exception {
        public OffsetTooBigException() { super(); }
        public OffsetTooBigException(String message) { super(message); }
    }

    protected void checkOffset16(int offset) throws OffsetTooBigException {
        if (offset < Short.MIN_VALUE || offset > Short.MAX_VALUE)
            throw new OffsetTooBigException("offset too big to fit"
                                            + " in 16 bits: " + offset);
    }

    public class Label {
        protected boolean anchored = false;
        protected int targetPC = 0;

        public void anchorToNext() {
            assert !anchored;
            this.targetPC = getPC();
            anchored = true;
        }

        public int getAnchor() {
            assert anchored;
            return targetPC;
        }

        protected int getOffset16(int pc, int instrPC)
            throws OffsetTooBigException {
            if (anchored) {
                int offset = targetPC - instrPC;
                checkOffset16(offset);
                return offset;
            } else {
                recordOffsetToPatch(pc, 16, instrPC, this);
                return 0;
            }
        }

        protected int getOffset32(int pc, int instrPC) {
            if (anchored)
                return targetPC - instrPC;
            else {
                recordOffsetToPatch(pc, 32, instrPC, this);
                return 0;
            }
        }
    }

    public Label newLabel() {
        return new Label();
    }

    public Label[] newLabels(int count) {
        Label[] labels = new Label[count];
        for (int i = 0; i < labels.length; ++i)
            labels[i] = newLabel();
        return labels;
    }

    protected static class OffsetToPatch {
        public final int pc;
        public final int size;
        public final int instrPC;
        public final Label label;

        public OffsetToPatch(int pc, int size, int instrPC, Label label) {
            this.pc = pc;
            this.size = size;
            this.instrPC = instrPC;
            this.label = label;
        }
    }

    protected void recordOffsetToPatch(int offsetPC,
                                       int size,
                                       int instrPC,
                                       Label label) {
        offsetToPatch.add(new OffsetToPatch(offsetPC, size, instrPC, label));
    }

    protected void patchAllOffset() throws OffsetTooBigException {
        Iterator offsetIt = offsetToPatch.iterator();
        while (offsetIt.hasNext()) {
            OffsetToPatch offset = (OffsetToPatch)offsetIt.next();
            int offsetValue = offset.label.getAnchor() - offset.instrPC;
            if (offset.size == 16) {
                checkOffset16(offsetValue);
                codeArray.putU2(offset.pc, offsetValue);
            } else
                codeArray.putU4(offset.pc, offsetValue);
        }
    }

    // Exception handling
    //////////////////////////////////////////////////////////////////////

    public class ExceptionHandler {
        protected int startPC, endPC, handlerPC;
        protected final String catchType;
        protected final int catchTypeIndex;

        public void setStartPC(int pc) {
            this.startPC = pc;
        }

        public int getStartPC() {
            return this.startPC;
        }

        public void setEndPC(int pc) {
            this.endPC = pc;
        }

        public int getEndPC() {
            return this.endPC;
        }

        public void setHandlerPC(int pc) {
            this.handlerPC = pc;
        }

        public int getHandlerPC() {
            return this.handlerPC;
        }

        public ExceptionHandler(String catchType) {
            this(0, 0, 0, catchType);
        }

        public ExceptionHandler(int startPC,
                                   int endPC,
                                   int handlerPC,
                                   String catchType) {
            this.startPC = startPC;
            this.endPC = endPC;
            this.handlerPC = handlerPC;
            this.catchType = catchType;
            this.catchTypeIndex = (catchType == null
                                   ? 0
                                   : pool.addClass(catchType));
        }

        public ExceptionHandler(DataInputStream stream) throws IOException {
            this.startPC = stream.readShort();
            this.endPC = stream.readShort();
            this.handlerPC = stream.readShort();
            this.catchTypeIndex = stream.readShort();
            this.catchType = (catchTypeIndex == 0
                              ? null
                              : pool.lookupClass(catchTypeIndex));
        }

        public void writeTo(DataOutputStream stream) throws IOException {
            stream.writeShort(startPC);
            stream.writeShort(endPC);
            stream.writeShort(handlerPC);
            stream.writeShort(catchTypeIndex);
        }

        // Follows javap output format for exception handlers.
        /*@Override*/public String toString() {
            StringBuffer buf = new StringBuffer("    ");
            if (startPC < 10) buf.append(" ");
            buf.append(startPC);
            buf.append("    ");
            if (endPC < 10) buf.append(" ");
            buf.append(endPC);
            buf.append("    ");
            buf.append(handlerPC);
            buf.append("   ");
            if (catchType != null) {
                buf.append("Class ");
                buf.append(catchType);
            }
            else
                buf.append("any");
            return buf.toString();
        }

    }

    public void addExceptionHandler(ExceptionHandler handler) {
        assert !frozen;
        exceptionHandlers.add(handler);
    }

    public void addExceptionHandler(int startPC,
                                    int endPC,
                                    int handlerPC,
                                    String catchType) {
        addExceptionHandler(new ExceptionHandler(startPC,
                                                 endPC,
                                                 handlerPC,
                                                 catchType));
    }

    public void addFinallyHandler(int startPC, int endPC, int handlerPC) {
        assert !frozen;
        addExceptionHandler(startPC, endPC, handlerPC, null);
    }

    public List/*<ExceptionHandler>*/ getExceptionHandlers() {
        return exceptionHandlers;
    }

    // Line numbers
    //////////////////////////////////////////////////////////////////////

    protected int[] lineNumbers = null;
    protected void ensureLineNumberCapacity(int endPC) {
        assert !frozen;
        if (lineNumbers == null) {
            lineNumbers = new int[endPC];
            addAttribute(context.JLineNumberTableAttribute(owner.getOwner(),
                                                           this));
        } else if (lineNumbers.length < endPC) {
            int[] newLN = new int[Math.max(endPC, lineNumbers.length * 2)];
            System.arraycopy(lineNumbers, 0, newLN, 0, lineNumbers.length);
            lineNumbers = newLN;
        }
    }

    /**
     * Set all line numbers in the interval [startPC, endPC) to
     * line, overwriting existing line numbers.
     */
    public void setLineNumber(int startPC, int endPC, int line) {
        ensureLineNumberCapacity(endPC);
        Arrays.fill(lineNumbers, startPC, endPC, line);
    }

    public void setLineNumber(int instrPC, int line) {
        setLineNumber(instrPC, instrPC + 1, line);
    }

    /** Sets all non-filled line numbers in the interval [startPC, endPC)
     *  to 'line'.
     */
    public void completeLineNumber(int startPC, int endPC, int line) {
        ensureLineNumberCapacity(endPC);
        for (int pc = startPC; pc < endPC; ++pc)
            if (lineNumbers[pc] == 0) lineNumbers[pc] = line;
    }

    public int[] getLineNumbers() {
        assert frozen;
        if (lineNumbers == null) return new int[0];
        else if (lineNumbers.length == getPC()) return lineNumbers;
        else {
            int[] trimmedLN = new int[getPC()];
            System.arraycopy(lineNumbers, 0,
                             trimmedLN, 0,
                             Math.min(lineNumbers.length, trimmedLN.length));
            return trimmedLN;
        }
    }

    // Output
    //////////////////////////////////////////////////////////////////////

    public void writeTo(DataOutputStream stream) throws IOException {
        assert frozen;
        stream.writeInt(getSize());
        codeArray.writeTo(stream);
    }

    // Follows javap output format for opcodes.
    /*@Override*/ public String toString() {
        StringBuffer buf = new StringBuffer();
        JOpcode opcode = null;
        int pc = 0, addr = 0;
        while (pc < codeArray.getSize()) {
            buf.append("\n   ");
            buf.append(pc);
            buf.append(":\t");
            opcode = JOpcode.OPCODES[codeArray.getU1(pc)];
            buf.append(decode(opcode, pc));
            if (opcode.code == JOpcode.cTABLESWITCH ||
                opcode.code == JOpcode.cLOOKUPSWITCH) {
                addr = ((pc / 4 + 1) + 1) * 4; // U4 aligned data
                int low = codeArray.getU4(addr);
                int high = codeArray.getU4(addr+4);
                pc = addr + (2/*low+high*/ + (high - low + 1)/*targets*/) * 4;
            } else
                pc += opcode.getSize();
        }
        if (exceptionHandlers.size() > 0) {
            buf.append("\n  Exception table:\n   from   to  target type\n");
            Iterator it = exceptionHandlers.iterator();
            while (it.hasNext()) {
                ExceptionHandler exh = (ExceptionHandler)it.next();
                buf.append(exh);
                buf.append("\n");
            }
        }
        return buf.toString();
    }

    private String decode(JOpcode opcode, int pc) {
        String ownerClassName = owner.getOwner().getName();
        int data, data2;
        StringBuilder buf = new StringBuilder();
        buf.append(opcode.name.toLowerCase());
        switch (opcode.code) {
        case JOpcode.cALOAD: case JOpcode.cASTORE: case JOpcode.cBIPUSH:
        case JOpcode.cDLOAD: case JOpcode.cDSTORE:
        case JOpcode.cFLOAD: case JOpcode.cFSTORE:
        case JOpcode.cILOAD: case JOpcode.cISTORE:
        case JOpcode.cLLOAD: case JOpcode.cLSTORE:
            data = codeArray.getU1(pc+1);
            buf.append("\t");
            buf.append(data);
            break;
        case JOpcode.cLDC:
            data = codeArray.getU1(pc+1);
            buf.append("\t#");
            buf.append(data);
            buf.append("; ");
            buf.append(pool.lookupEntry(data).toComment(ownerClassName));
            break;
        case JOpcode.cNEWARRAY:
            data = codeArray.getU1(pc+1);
            buf.append(" ");
            buf.append(JType.tagToString(data));
            break;
        case JOpcode.cIINC:
            data = codeArray.getU1(pc+1);
            data2 = codeArray.getU1(pc+2);
            buf.append("\t");
            buf.append(data);
            buf.append(", ");
            buf.append(data2);
            break;
        case JOpcode.cSIPUSH:
            data = codeArray.getU2(pc+1);
            buf.append("\t");
            buf.append(data);
            break;
        case JOpcode.cANEWARRAY: case JOpcode.cCHECKCAST:
        case JOpcode.cGETFIELD: case JOpcode.cGETSTATIC:
        case JOpcode.cINSTANCEOF:
        case JOpcode.cINVOKESPECIAL: case JOpcode.cINVOKESTATIC:
        case JOpcode.cINVOKEVIRTUAL:
        case JOpcode.cLDC_W: case JOpcode.cLDC2_W: case JOpcode.cNEW:
        case JOpcode.cPUTFIELD: case JOpcode.cPUTSTATIC:
            data = codeArray.getU2(pc+1);
            buf.append("\t#");
            buf.append(data);
            buf.append("; ");
            buf.append(pool.lookupEntry(data).toComment(ownerClassName));
            break;
        case JOpcode.cIF_ACMPEQ: case JOpcode.cIF_ACMPNE:
        case JOpcode.cIFEQ: case JOpcode.cIFGE: case JOpcode.cIFGT:
        case JOpcode.cIFLE: case JOpcode.cIFLT: case JOpcode.cIFNE:
        case JOpcode.cIFNONNULL: case JOpcode.cIFNULL:
        case JOpcode.cIF_ICMPEQ: case JOpcode.cIF_ICMPGE:
        case JOpcode.cIF_ICMPGT: case JOpcode.cIF_ICMPLE:
        case JOpcode.cIF_ICMPLT: case JOpcode.cIF_ICMPNE:
            data = codeArray.getU2(pc+1); // maybe S2 offset
            buf.append("\t");
            buf.append(pc+data);
            break;
        case JOpcode.cGOTO:
            data = codeArray.getS2(pc+1); // always S2 offset
            buf.append("\t");
            buf.append(pc+data);
            break;
        case JOpcode.cINVOKEINTERFACE:
            data = codeArray.getU2(pc+1);
            data2 = codeArray.getU1(pc+3);
            buf.append("\t#");
            buf.append(data);
            buf.append(",  ");
            buf.append(data2);
            buf.append("; ");
            buf.append(pool.lookupEntry(data).toComment(ownerClassName));
            break;
        case JOpcode.cTABLESWITCH:
            buf.append("{ //");
            int addr = ((pc / 4 + 1) + 1) * 4; // U4 aligned data
            int low = codeArray.getU4(addr);
            int high = codeArray.getU4(addr+4);
            buf.append(low);
            buf.append(" to ");
            buf.append(high);
            for (int i = low; i <= high; ++i) {
                buf.append("\n\t\t");
                buf.append(i);
                buf.append(": ");
                buf.append(pc+codeArray.getU4(addr+(i-1)*4));
                buf.append(";");
            }
            buf.append("\n\t\tdefault: ");
            buf.append(pc+codeArray.getU4(addr-4));
            buf.append(" }");
        default:
        }
        return buf.toString();
    }
}
