/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

/**
 * Extended list of instructions, providing pseudo-instructions which
 * are easier to use than the standard ones.
 *
 * @author Michel Schinz, Thomas Friedli
 * @version 1.0
 */

public class JExtendedCode extends JCode {
    public final static int COND_EQ = 0;
    public final static int COND_NE = 1;
    public final static int COND_LT = 2;
    public final static int COND_GE = 3;
    public final static int COND_GT = 4;
    public final static int COND_LE = 5;

    private final JOpcode[] forbidden = new JOpcode[0];
    private final JOpcode[] nothingToDo = new JOpcode[0];

    private final JOpcode[][][] typeConversions = {
        {
            /* T_BOOLEAN -> T_BOOLEAN */ nothingToDo,
            /* T_BOOLEAN -> T_CHAR */    forbidden,
            /* T_BOOLEAN -> T_FLOAT */   forbidden,
            /* T_BOOLEAN -> T_DOUBLE */  forbidden,
            /* T_BOOLEAN -> T_BYTE */    forbidden,
            /* T_BOOLEAN -> T_SHORT */   forbidden,
            /* T_BOOLEAN -> T_INT */     forbidden,
            /* T_BOOLEAN -> T_LONG */    forbidden
        },
        {
            /* T_CHAR -> T_BOOLEAN */ forbidden,
            /* T_CHAR -> T_CHAR */    nothingToDo,
            /* T_CHAR -> T_FLOAT */   {JOpcode.I2F},
            /* T_CHAR -> T_DOUBLE */  {JOpcode.I2D},
            /* T_CHAR -> T_BYTE */    {JOpcode.I2B},
            /* T_CHAR -> T_SHORT */   {JOpcode.I2S},
            /* T_CHAR -> T_INT */     nothingToDo,
            /* T_CHAR -> T_LONG */    {JOpcode.I2L}
        },
        {
            /* T_FLOAT -> T_BOOLEAN */ forbidden,
            /* T_FLOAT -> T_CHAR */    {JOpcode.F2I, JOpcode.I2C},
            /* T_FLOAT -> T_FLOAT */   nothingToDo,
            /* T_FLOAT -> T_DOUBLE */  {JOpcode.F2D},
            /* T_FLOAT -> T_BYTE */    {JOpcode.F2I, JOpcode.I2B},
            /* T_FLOAT -> T_SHORT */   {JOpcode.F2I, JOpcode.I2S},
            /* T_FLOAT -> T_INT */     {JOpcode.F2I},
            /* T_FLOAT -> T_LONG */    {JOpcode.F2L}
        },
        {
            /* T_DOUBLE -> T_BOOLEAN */ forbidden,
            /* T_DOUBLE -> T_CHAR */    {JOpcode.D2I, JOpcode.I2C},
            /* T_DOUBLE -> T_FLOAT */   {JOpcode.D2F},
            /* T_DOUBLE -> T_DOUBLE */  nothingToDo,
            /* T_DOUBLE -> T_BYTE */    {JOpcode.D2I, JOpcode.I2B},
            /* T_DOUBLE -> T_SHORT */   {JOpcode.D2I, JOpcode.I2S},
            /* T_DOUBLE -> T_INT */     {JOpcode.D2I},
            /* T_DOUBLE -> T_LONG */    {JOpcode.D2L}
        },
        {
            /* T_BYTE -> T_BOOLEAN */ forbidden,
            /* T_BYTE -> T_CHAR */    {JOpcode.I2C},
            /* T_BYTE -> T_FLOAT */   {JOpcode.I2F},
            /* T_BYTE -> T_DOUBLE */  {JOpcode.I2D},
            /* T_BYTE -> T_BYTE */    nothingToDo,
            /* T_BYTE -> T_SHORT */   nothingToDo,
            /* T_BYTE -> T_INT */     nothingToDo,
            /* T_BYTE -> T_LONG */    {JOpcode.I2L}
        },
        {
            /* T_SHORT -> T_BOOLEAN */ forbidden,
            /* T_SHORT -> T_CHAR */    {JOpcode.I2C},
            /* T_SHORT -> T_FLOAT */   {JOpcode.I2F},
            /* T_SHORT -> T_DOUBLE */  {JOpcode.I2D},
            /* T_SHORT -> T_BYTE */    {JOpcode.I2B},
            /* T_SHORT -> T_SHORT */   nothingToDo,
            /* T_SHORT -> T_INT */     nothingToDo,
            /* T_SHORT -> T_LONG */    {JOpcode.I2L}
        },
        {
            /* T_INT -> T_BOOLEAN */ forbidden,
            /* T_INT -> T_CHAR */    {JOpcode.I2C},
            /* T_INT -> T_FLOAT */   {JOpcode.I2F},
            /* T_INT -> T_DOUBLE */  {JOpcode.I2D},
            /* T_INT -> T_BYTE */    {JOpcode.I2B},
            /* T_INT -> T_SHORT */   {JOpcode.I2S},
            /* T_INT -> T_INT */     nothingToDo,
            /* T_INT -> T_LONG */    {JOpcode.I2L}
        },
        {
            /* T_LONG -> T_BOOLEAN */ forbidden,
            /* T_LONG -> T_CHAR */    {JOpcode.L2I, JOpcode.I2C},
            /* T_LONG -> T_FLOAT */   {JOpcode.L2F},
            /* T_LONG -> T_DOUBLE */  {JOpcode.L2D},
            /* T_LONG -> T_BYTE */    {JOpcode.L2I, JOpcode.I2B},
            /* T_LONG -> T_SHORT */   {JOpcode.L2I, JOpcode.I2S},
            /* T_LONG -> T_INT */     {JOpcode.L2I},
            /* T_LONG -> T_LONG */    nothingToDo
        }
    };

    public JExtendedCode(FJBGContext context,
                         JClass clazz,
                         JMethod owner) {
        super(context, clazz, owner);
    }

    public void emitPUSH(boolean value) { emitPUSH(value ? 1 : 0); }
    public void emitPUSH(Boolean value) { emitPUSH(value.booleanValue()); }

    public void emitPUSH(byte value) {
      switch (value) {
        case -1: emitICONST_M1(); break;
        case 0: emitICONST_0(); break;
        case 1: emitICONST_1(); break;
        case 2: emitICONST_2(); break;
        case 3: emitICONST_3(); break;
        case 4: emitICONST_4(); break;
        case 5: emitICONST_5(); break;
        default:
          emitBIPUSH(value);
      }
    }
    public void emitPUSH(Byte value) { emitPUSH(value.byteValue()); }

    public void emitPUSH(short value) {
      switch (value) {
        case -1: emitICONST_M1(); break;
        case 0: emitICONST_0(); break;
        case 1: emitICONST_1(); break;
        case 2: emitICONST_2(); break;
        case 3: emitICONST_3(); break;
        case 4: emitICONST_4(); break;
        case 5: emitICONST_5(); break;
        default:
          if (value >= Byte.MIN_VALUE && value <= Byte.MAX_VALUE)
            emitBIPUSH((byte)value);
          else
            emitSIPUSH(value);
      }
    }
    public void emitPUSH(Short value) { emitPUSH(value.shortValue()); }

    // TODO check that we do the right thing here
    public void emitPUSH(char value) { emitPUSH((int)value); }
    public void emitPUSH(Character value) { emitPUSH(value.charValue()); }

    public void emitPUSH(int value) {
        switch (value) {
        case -1: emitICONST_M1(); break;
        case 0: emitICONST_0(); break;
        case 1: emitICONST_1(); break;
        case 2: emitICONST_2(); break;
        case 3: emitICONST_3(); break;
        case 4: emitICONST_4(); break;
        case 5: emitICONST_5(); break;
        default:
          if (value >= Byte.MIN_VALUE && value <= Byte.MAX_VALUE)
            emitBIPUSH((byte)value);
          else if (value >= Short.MIN_VALUE && value <= Short.MAX_VALUE)
            emitSIPUSH((short)value);
          else
            emitPUSH_index(pool.addInteger(value));
          break;
        }
    }
    public void emitPUSH(Integer value) { emitPUSH(value.intValue()); }

    public void emitPUSH(long value) {
        if (value == 0L)
            emitLCONST_0();
        else if (value == 1L)
            emitLCONST_1();
        else
            emitLDC2_W(value);
    }
    public void emitPUSH(Long value) { emitPUSH(value.longValue()); }

    private static final Float ZEROF = Float.valueOf(0f);
    private static final Float ONEF = Float.valueOf(1f);
    private static final Float TWOF = Float.valueOf(2f);
    public void emitPUSH(Float value) {
        if (ZEROF.equals(value))
            emitFCONST_0();
        else if (ONEF.equals(value))
            emitFCONST_1();
        else if (TWOF.equals(value))
            emitFCONST_2();
        else
            emitPUSH_index(pool.addFloat(value.floatValue()));
    }
    public void emitPUSH(float value) { emitPUSH(Float.valueOf(value)); }

    private static final Double ZEROD = Double.valueOf(0d);
    private static final Double ONED = Double.valueOf(1d);
    public void emitPUSH(Double value) {
        if (ZEROD.equals(value))
            emitDCONST_0();
        else if (ONED.equals(value))
            emitDCONST_1();
        else
            emitLDC2_W(value.doubleValue());
    }
    public void emitPUSH(double value) { emitPUSH(Double.valueOf(value)); }

    public void emitPUSH(String s) {
        emitPUSH_index(pool.addString(s));
    }

    /** Pushes a class literal on the stack */
    public void emitPUSH(JReferenceType type) {
        assert owner.owner.major >= 49;
        emitPUSH_index(pool.addClass(type.getDescriptor()));
    }

    protected void emitPUSH_index(int index) {
        if (index <= 0xFF)
            emitU1(JOpcode.LDC, index);
        else
            emitU2(JOpcode.LDC_W, index);
    }

    public void emitLOAD(int index, JType type) {
        JOpcode opcode;

        switch (type.getTag()) {
        case JType.T_BOOLEAN: case JType.T_BYTE: case JType.T_CHAR:
        case JType.T_SHORT: case JType.T_INT:
            switch (index) {
            case 0: emitILOAD_0(); return;
            case 1: emitILOAD_1(); return;
            case 2: emitILOAD_2(); return;
            case 3: emitILOAD_3(); return;
            default: opcode = JOpcode.ILOAD;
            } break;
        case JType.T_FLOAT:
            switch (index) {
            case 0: emitFLOAD_0(); return;
            case 1: emitFLOAD_1(); return;
            case 2: emitFLOAD_2(); return;
            case 3: emitFLOAD_3(); return;
            default: opcode = JOpcode.FLOAD;
            } break;
        case JType.T_LONG:
            switch (index) {
            case 0: emitLLOAD_0(); return;
            case 1: emitLLOAD_1(); return;
            case 2: emitLLOAD_2(); return;
            case 3: emitLLOAD_3(); return;
            default: opcode = JOpcode.LLOAD;
            } break;
        case JType.T_DOUBLE:
            switch (index) {
            case 0: emitDLOAD_0(); return;
            case 1: emitDLOAD_1(); return;
            case 2: emitDLOAD_2(); return;
            case 3: emitDLOAD_3(); return;
            default: opcode = JOpcode.DLOAD;
            } break;
        case JType.T_ARRAY: case JType.T_OBJECT:
            switch (index) {
            case 0: emitALOAD_0(); return;
            case 1: emitALOAD_1(); return;
            case 2: emitALOAD_2(); return;
            case 3: emitALOAD_3(); return;
            default: opcode = JOpcode.ALOAD;
            } break;
        default:
            throw new IllegalArgumentException("invalid type for load "+type);
        }

        if (index > 0xFF)
            emitWIDE(opcode, index);
        else
            emitU1(opcode, index);
    }
    public void emitLOAD(JLocalVariable var) {
        emitLOAD(var.index, var.type);
    }

    public void emitSTORE(int index, JType type) {
        JOpcode opcode;

        switch (type.getTag()) {
        case JType.T_BOOLEAN: case JType.T_BYTE: case JType.T_CHAR:
        case JType.T_SHORT: case JType.T_INT:
            switch (index) {
            case 0: emitISTORE_0(); return;
            case 1: emitISTORE_1(); return;
            case 2: emitISTORE_2(); return;
            case 3: emitISTORE_3(); return;
            default: opcode = JOpcode.ISTORE;
            } break;
        case JType.T_FLOAT:
            switch (index) {
            case 0: emitFSTORE_0(); return;
            case 1: emitFSTORE_1(); return;
            case 2: emitFSTORE_2(); return;
            case 3: emitFSTORE_3(); return;
            default: opcode = JOpcode.FSTORE;
            } break;
        case JType.T_LONG:
            switch (index) {
            case 0: emitLSTORE_0(); return;
            case 1: emitLSTORE_1(); return;
            case 2: emitLSTORE_2(); return;
            case 3: emitLSTORE_3(); return;
            default: opcode = JOpcode.LSTORE;
            } break;
        case JType.T_DOUBLE:
            switch (index) {
            case 0: emitDSTORE_0(); return;
            case 1: emitDSTORE_1(); return;
            case 2: emitDSTORE_2(); return;
            case 3: emitDSTORE_3(); return;
            default: opcode = JOpcode.DSTORE;
            } break;
        case JType.T_ARRAY: case JType.T_OBJECT: case JType.T_ADDRESS:
            switch (index) {
            case 0: emitASTORE_0(); return;
            case 1: emitASTORE_1(); return;
            case 2: emitASTORE_2(); return;
            case 3: emitASTORE_3(); return;
            default: opcode = JOpcode.ASTORE;
            } break;
        default:
            throw new IllegalArgumentException("invalid type for store "+type);
        }

        if (index > 0xFF)
            emitWIDE(opcode, index);
        else
            emitU1(opcode, index);
    }
    public void emitSTORE(JLocalVariable var) {
        emitSTORE(var.index, var.type);
    }

    public void emitALOAD(JType type) {
        switch (type.getTag()) {
        case JType.T_BOOLEAN:
        case JType.T_BYTE:
            emitBALOAD();
            break;
        case JType.T_CHAR:
            emitCALOAD();
            break;
        case JType.T_SHORT:
            emitSALOAD();
            break;
        case JType.T_INT:
            emitIALOAD();
            break;
        case JType.T_FLOAT:
            emitFALOAD();
            break;
        case JType.T_LONG:
            emitLALOAD();
            break;
        case JType.T_DOUBLE:
            emitDALOAD();
            break;
        case JType.T_ARRAY:
        case JType.T_OBJECT:
            emitAALOAD();
            break;
        default:
            throw new IllegalArgumentException("invalid type for aload " + type);
        }
    }

    public void emitASTORE(JType type) {
        switch (type.getTag()) {
        case JType.T_BOOLEAN:
        case JType.T_BYTE:
            emitBASTORE();
            break;
        case JType.T_CHAR:
            emitCASTORE();
            break;
        case JType.T_SHORT:
            emitSASTORE();
            break;
        case JType.T_INT:
            emitIASTORE();
            break;
        case JType.T_FLOAT:
            emitFASTORE();
            break;
        case JType.T_LONG:
            emitLASTORE();
            break;
        case JType.T_DOUBLE:
            emitDASTORE();
            break;
        case JType.T_ARRAY:
        case JType.T_OBJECT:
            emitAASTORE();
            break;
        default:
            throw new IllegalArgumentException("invalid type for astore " + type);
        }
    }

    public void emitRETURN(JType type) {
        if (type.isValueType()) {
            switch (type.getTag()) {
            case JType.T_BOOLEAN:
            case JType.T_BYTE:
            case JType.T_CHAR:
            case JType.T_SHORT:
            case JType.T_INT:
                emitIRETURN();
                break;
            case JType.T_FLOAT:
                emitFRETURN();
                break;
            case JType.T_LONG:
                emitLRETURN();
                break;
            case JType.T_DOUBLE:
                emitDRETURN();
                break;
            }
        } else if (type.isArrayType() || type.isObjectType())
            emitARETURN();
        else if (type == JType.VOID)
            emitRETURN();
        else
            throw new IllegalArgumentException("invalid type for RETURN " + type);
    }

    public void emitADD(JType type) {
        switch (type.getTag()) {
        case JType.T_BOOLEAN: case JType.T_BYTE: case JType.T_CHAR:
        case JType.T_SHORT: case JType.T_INT:
            emitIADD(); break;
        case JType.T_FLOAT:
            emitFADD(); break;
        case JType.T_LONG:
            emitLADD(); break;
        case JType.T_DOUBLE:
            emitDADD(); break;
        }
    }

    /**
     * Emits a basic type conversion instruction choosen according to the
     * types given in parameter.
     *
     * @param fromType The type of the value to be cast into another type.
     * @param toType The type the value will be cast into.
     */
    public void emitT2T(JType fromType, JType toType) {
        assert fromType.getTag() >= JType.T_BOOLEAN
            && fromType.getTag() <= JType.T_LONG
            && toType.getTag() >= JType.T_BOOLEAN
            && toType.getTag() <= JType.T_LONG;

        JOpcode[] conv = typeConversions[fromType.getTag() - 4][toType.getTag() - 4];
        if (conv == forbidden) {
            throw new Error("inconvertible types : " + fromType.toString()
                            + " -> " + toType.toString());
        } else if (conv != nothingToDo) {
            for (int i = 0; i < conv.length; i++) {
                emit(conv[i]);
            }
        }
    }

    public void emitIF(int cond, Label label) throws OffsetTooBigException {
        assert cond >= COND_EQ && cond <= COND_LE;
        emitU2(JOpcode.OPCODES[153 + cond], label.getOffset16(getPC() + 1, getPC()));
    }
    public void emitIF(int cond, int targetPC) throws OffsetTooBigException {
        int offset = targetPC - getPC();
        emitU2(JOpcode.OPCODES[153 + cond], offset);
    }
    public void emitIF(int cond) throws OffsetTooBigException {
        emitIF(cond, 0);
    }

    public void emitIF_ICMP(int cond, Label label) throws OffsetTooBigException {
        assert cond >= COND_EQ && cond <= COND_LE;
        emitU2(JOpcode.OPCODES[159 + cond], label.getOffset16(getPC() + 1, getPC()));
    }
    public void emitIF_ICMP(int cond, int targetPC) throws OffsetTooBigException {
        int offset = targetPC - getPC();
        emitU2(JOpcode.OPCODES[159 + cond], offset);
    }
    public void emitIF_ICMP(int cond) throws OffsetTooBigException {
        emitIF_ICMP(cond, 0);
    }

    public void emitIF_ACMP(int cond, Label label) throws OffsetTooBigException {
        assert cond == COND_EQ || cond == COND_NE;
        emitU2(JOpcode.OPCODES[165 + cond], label.getOffset16(getPC() + 1, getPC()));
    }
    public void emitIF_ACMP(int cond, int targetPC) throws OffsetTooBigException {
        int offset = targetPC - getPC();
        emitU2(JOpcode.OPCODES[165 + cond], offset);
    }
    public void emitIF_ACMP(int cond) throws OffsetTooBigException {
        emitIF_ACMP(cond, 0);
    }

    public void emitGOTO_maybe_W(Label label, boolean defaultToWide) {
        if (label.anchored)
            emitGOTO_maybe_W(label.targetPC);
        else {
            if (defaultToWide)
                emitGOTO_W(label);
            else {
                try {
                    emitGOTO(label);
                } catch (OffsetTooBigException e) {
                    throw new Error(e);
                }
            }
        }
    }

    public void emitGOTO_maybe_W(int targetPC) {
        int offset = targetPC - (getPC() + 1);
        if (offset < Short.MIN_VALUE || offset > Short.MAX_VALUE)
            emitGOTO_W(targetPC);
        else {
            try {
                emitGOTO(targetPC);
            } catch (OffsetTooBigException e) {
                throw new Error(e);
            }
        }
    }

    /**
     * Emits a switch instruction choosen according to the caracteristics
     * of the given list of keys and a default maxRate.
     *
     * @param keySets The array of all keys that must be compared to the
     *        value on stack.
     * @param branches The labels representing the jump addresses linked
     *        with the corresponding keys.
     * @param defaultBranch The label representing the default branch
     *        address.
     */
    public void emitSWITCH(int[][] keySets,
                           Label[] branches,
                           Label defaultBranch,
                           double minDensity) {
        assert keySets.length == branches.length;

        int flatSize = 0;
        for (int i = 0; i < keySets.length; ++i)
            flatSize += keySets[i].length;

        int[] flatKeys = new int[flatSize];
        Label[] flatBranches = new Label[flatSize];
        int flatI = 0;
        for (int i = 0; i < keySets.length; ++i) {
            Label branch = branches[i];
            int[] keys = keySets[i];
            for (int j = 0; j < keys.length; ++j) {
                flatKeys[flatI] = keys[j];
                flatBranches[flatI] = branch;
            }
            ++flatI;
        }
        assert flatI == flatSize;
        emitSWITCH(flatKeys, flatBranches, defaultBranch, minDensity);
    }

    /**
     * Emits a switch instruction choosen according to the caracteristics
     * of the given list of keys and a given maxRate.
     *
     * @param keys The array of all keys that must be compared to the
     *        value on stack.
     * @param branches The labels representing the jump addresses linked
     *        with the corresponding keys.
     * @param defaultBranch The label representing the default branch
     *        address.
     * @param minDensity The minimum density to use for TABLESWITCH.
     */
    public void emitSWITCH(int[] keys,
                           Label[] branches,
                           Label defaultBranch,
                           double minDensity) {
        assert keys.length == branches.length;

        //The special case for empty keys. It makes sense to allow
        //empty keys and generate LOOKUPSWITCH with defaultBranch
        //only. This is exactly what javac does for switch statement
        //that has only a default case.
        if (keys.length == 0) {
          emitLOOKUPSWITCH(keys, branches, defaultBranch);
          return;
        }
        //the rest of the code assumes that keys.length > 0

        // sorting the tables
        // FIXME use quicksort
        for (int i = 1; i < keys.length; i++) {
            for (int j = 1; j <= keys.length - i; j++) {
                if (keys[j] < keys[j - 1]) {
                    int tmp = keys[j];
                    keys[j] = keys[j - 1];
                    keys[j - 1] = tmp;

                    Label tmp_l = branches[j];
                    branches[j] = branches[j - 1];
                    branches[j - 1] = tmp_l;
                }
            }
        }

        int keyMin = keys[0], keyMax = keys[keys.length - 1];
        /** Calculate in long to guard against overflow. */
        long keyRange = (long)keyMax - keyMin + 1;
        if ((double)keys.length / (double)keyRange >= minDensity) {
            // Keys are dense enough, use a table in which holes are
            // filled with defaultBranch.
            int[] newKeys = new int[(int)keyRange];
            Label[] newBranches = new Label[(int)keyRange];
            int oldPos = 0;
            for (int i = 0; i < keyRange; ++i) {
                int key = keyMin + i;
                newKeys[i] = key;
                if (keys[oldPos] == key) {
                    newBranches[i] = branches[oldPos];
                    ++oldPos;
                } else
                    newBranches[i] = defaultBranch;
            }
            assert oldPos == keys.length;
            emitTABLESWITCH(newKeys, newBranches, defaultBranch);
        } else
            emitLOOKUPSWITCH(keys, branches, defaultBranch);
    }

    /**
     * Emits a method invocation instruction choosen according to
     * the caracteristics of the given method.
     *
     * @param method The method to be invoked.
     */
    public void emitINVOKE(JMethod method) {
        String mName = method.getName();
        String cName = method.getOwner().getName();
        JMethodType mType = (JMethodType)method.getType();
        if (method.isStatic())
            emitINVOKESTATIC(cName, mName, mType);
        else if (method.getOwner().isInterface())
            emitINVOKEINTERFACE(cName, mName, mType);
        else
            emitINVOKEVIRTUAL(cName, mName, mType);
    }

}
