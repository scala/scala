/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import ch.epfl.lamp.util.ByteArray;

/**
 * Iterator used to examine the contents of an instruction list.
 *
 * @author Michel Schinz, Thomas Friedli
 * @version 1.0
 */

public class JCodeIterator {
    protected final JCode code;
    protected final JConstantPool pool;
    protected final ByteArray codeArray;

    protected int pc;
    protected JOpcode opcode;

    /**
     * Creates a new code iterator with its instruction list
     * and its pc initialized to a given value.
     */
    public JCodeIterator(JCode code, int pc) {
        this.code = code;
        this.pool = code.getOwner().getOwner().getConstantPool();
        this.codeArray = code.codeArray;
        this.pc = pc;
        setOpcode();
    }

    public JCodeIterator(JCode code) {
        this(code, 0);
    }

    /**
     * Get the current program counter.
     * @return The current program counter.
     */
    public int getPC() { return pc; }

    /**
     * Searches the type of the instruction positionned at the
     * current address and updates the current instruction.
     */
    protected void setOpcode() {
        // TODO : check if the current pc is the beginning
        //        of an instruction
        opcode = isValid() ? JOpcode.OPCODES[codeArray.getU1(pc)] : null;
    }

    /**
     * Returns the opcode of the current instruction.
     * @return The opcode of the current instruction.
     */
    public JOpcode getOpcode() {
        return opcode;
    }

    /**
     * Updates the program counter to an given value.
     * @param pc The new value of the program counter.
     */
    public void moveTo(int pc) {
        this.pc = pc;
        setOpcode();
    }

    /**
     * Check the validity of the iterator.
     * @return true iff the iterator points to a valid address.
     */
    public boolean isValid() {
        return pc < codeArray.getSize();
    }

    /**
     * Updates the current instruction with the next one in the
     * sense of their position in the code.
     */
    public void moveToNext() {
        moveTo(pc + getInstructionSize());
    }

    /**
     * Updates the current instruction with a specific successor
     * of it.
     * @param succ The index of the wanted successor in the list of
     * the successors of the current instruction.
     */
    public void moveToSuccessor(int succ) {
        moveTo(getSuccessorPC(succ));
    }

    /**
     * Updates the current instruction with the one positionned
     * at a given index relatively to the actual program counter
     * @param offset The relative position of the instruction
     * compared with the position of the current one
     */
    public void moveRelatively(int offset) {
        moveTo(pc + offset);
    }

    /**
     * Returns the size in bytes of the current instruction.
     * @return The size in bytes of the current instruction.
     */
    public int getInstructionSize() {
        if (opcode.size != JOpcode.UNKNOWN) {
            return opcode.size;
        } else if (opcode == JOpcode.TABLESWITCH) {
            int lowOffset = 1 + pad4(pc + 1) + 4;
            int low = codeArray.getS4(pc + lowOffset);
            int high = codeArray.getS4(pc + lowOffset + 4);
            return lowOffset + 8 + 4 * (high - low + 1);
        } else if (opcode == JOpcode.LOOKUPSWITCH) {
            int npairsOffset = 1 + pad4(pc + 1) + 4;
            int npairs = codeArray.getS4(pc + npairsOffset);
            return npairsOffset + 4 + 8 * npairs;
        } else if (opcode == JOpcode.WIDE) {
            if (codeArray.getU1(pc + 1) == JOpcode.cIINC)
                return 6;
            else
                return 4;
        } else
            throw new Error("Unknown size for instruction " + opcode);
    }

    /**
     * Returns the number of successors of the current instruction.
     * @return The number of successors of the current instruction.
     */
    public int getSuccessorCount() {
        if (opcode.successorCount != JOpcode.UNKNOWN) {
            return opcode.successorCount;
        } else if (opcode == JOpcode.TABLESWITCH) {
            int lowPos = pc + 1 + pad4(pc + 1) + 4;
            return 1                           // default case
                + codeArray.getS4(lowPos + 4)  // value of HIGH field
                - codeArray.getS4(lowPos) + 1; // value of LOW field
        } else if (opcode == JOpcode.LOOKUPSWITCH) {
            int npairsPos = pc + 1 + pad4(pc + 1) + 4;
            return 1 + codeArray.getS4(npairsPos);
        } else
            throw new Error("Unknown successors for instruction " + opcode);
    }

    /**
     * Returns the address of the successor of the current instruction
     * given its index in the list of successors of the current
     * instruction.
     * @param index The index of the wanted successor in the list of
     * the successors of the current instruction.
     * @return The address of the specific successor.
     */
    public int getSuccessorPC(int index) {
        assert (index >= 0) && (index < getSuccessorCount()) : index;

        switch (opcode.jumpKind) {
        case JOpcode.JMP_NEXT:
            return pc + getInstructionSize();
        case JOpcode.JMP_ALWAYS_S2_OFFSET:
            return pc + codeArray.getS2(pc + 1);
        case JOpcode.JMP_ALWAYS_S4_OFFSET:
            return pc + codeArray.getS4(pc + 1);
        case JOpcode.JMP_MAYBE_S2_OFFSET:
            if (index == 0)
                return pc + getInstructionSize();
            else
                return pc + codeArray.getS2(pc + 1);
        case JOpcode.JMP_TABLE: {
            int defaultPos = pc + 1 + pad4(pc + 1);
            if (index == 0)
                return pc + codeArray.getS4(defaultPos);
            else
                return pc + codeArray.getS4(defaultPos + 3*4 + 4 * (index - 1));
        }
        case JOpcode.JMP_LOOKUP: {
            int defaultPos = pc + 1 + pad4(pc + 1);
            if (index == 0)
                return pc + codeArray.getS4(defaultPos);
            else
                return pc + codeArray.getS4(defaultPos + 2*4 + 4 + 8 * (index - 1));
        }
        default:
            throw new Error();
        }
    }

    /**
     * Returns the total size of data words put on the stack by the current
     * instruction.
     * @return The total size of data words put on the stack by the current
     * instruction.
     */
    public int getProducedDataSize() {
        if (opcode.getProducedDataTypes() == JOpcode.UNKNOWN_TYPE) {
            switch (opcode.code) {
            case JOpcode.cLDC: case JOpcode.cLDC_W: case JOpcode.cBALOAD:
                return 1;
            case JOpcode.cLDC2_W: case JOpcode.cDUP: case JOpcode.cSWAP:
                return 2;
            case JOpcode.cDUP_X1:
                return 3;
            case JOpcode.cDUP_X2: case JOpcode.cDUP2:
                return 4;
            case JOpcode.cDUP2_X1:
                return 5;
            case JOpcode.cDUP2_X2:
                return 6;
            case JOpcode.cGETSTATIC: case JOpcode.cGETFIELD: {
                JConstantPool.FieldOrMethodRefEntry entry =
                    (JConstantPool.FieldOrMethodRefEntry)
                    pool.lookupEntry(codeArray.getU2(pc + 1));
                return JType.parseSignature(entry.getSignature()).getSize();
            }
            case JOpcode.cWIDE : {
                int op = codeArray.getU1(pc + 1);
                if (op >= JOpcode.cILOAD && op <= JOpcode.cALOAD) {
                    JOpcode opcode2 = JOpcode.OPCODES[op];
                    return JType.getTotalSize(opcode2.getProducedDataTypes());
                } else if (op >= JOpcode.cISTORE && op <= JOpcode.cASTORE)
                    return 0;
                else return 0; // (IINC)
            }
            default :
                throw new Error(opcode.toString());
            }
        } else
            return JType.getTotalSize(opcode.getProducedDataTypes());
    }

    /**
     * Returns the total size of data words taken from the stack by the current
     * instruction.
     * @return The total size of data words taken from the stack by the current
     * instruction.
     */
    public int getConsumedDataSize() {
        if (opcode.getConsumedDataTypes() != JOpcode.UNKNOWN_TYPE)
            return JType.getTotalSize(opcode.getConsumedDataTypes());
        else {
            switch (opcode.code) {
            case JOpcode.cPOP: case JOpcode.cDUP:
                return 1;
            case JOpcode.cPOP2: case JOpcode.cSWAP:
            case JOpcode.cDUP_X1: case JOpcode.cDUP2:
                return 2;
            case JOpcode.cDUP_X2: case JOpcode.cDUP2_X1:
                return 3;
            case JOpcode.cDUP2_X2:
                return 4;
            case JOpcode.cPUTSTATIC: case JOpcode.cPUTFIELD: {
                JConstantPool.FieldOrMethodRefEntry entry =
                    (JConstantPool.FieldOrMethodRefEntry)
                    pool.lookupEntry(codeArray.getU2(pc + 1));
                return JType.parseSignature(entry.getSignature()).getSize();
            }
            case JOpcode.cINVOKEVIRTUAL: case JOpcode.cINVOKESPECIAL:
            case JOpcode.cINVOKESTATIC:  case JOpcode.cINVOKEINTERFACE : {
                JConstantPool.FieldOrMethodRefEntry entry =
                    (JConstantPool.FieldOrMethodRefEntry)
                    pool.lookupEntry(codeArray.getU2(pc + 1));
                JMethodType tp = (JMethodType)
                    JType.parseSignature(entry.getSignature());
                return tp.getArgsSize()
                    + (opcode == JOpcode.INVOKESTATIC ? 0 : 1);
            }
            case JOpcode.cWIDE : {
                int op = codeArray.getU1(pc + 1);
                if (op >= JOpcode.cILOAD && op <= JOpcode.cALOAD)
                    return 0;
                else if (op >= JOpcode.cISTORE && op <= JOpcode.cASTORE) {
                    JOpcode opcode2 = JOpcode.OPCODES[op];
                    return JType.getTotalSize(opcode2.getConsumedDataTypes());
                } else
                    return 0; // (IINC)
            }
            case JOpcode.cMULTIANEWARRAY :
                return codeArray.getU1(pc + 3);
            default:
                throw new Error(opcode.toString());
            }
        }
    }

    /**
     * Returns the number of data types put on the stack by the current
     * instruction.
     * @return The number of data types put on the stack by the current
     * instruction.
     */
    public int getProducedDataTypesNumber() {
        if (opcode.getProducedDataTypes() != JOpcode.UNKNOWN_TYPE)
            return opcode.getProducedDataTypes().length;
        else {
            switch (opcode.code) {
            case JOpcode.cLDC: case JOpcode.cLDC_W: case JOpcode.cLDC2_W:
            case JOpcode.cBALOAD: case JOpcode.cGETSTATIC:
            case JOpcode.cGETFIELD:
                return 1;
            case JOpcode.cDUP: case JOpcode.cSWAP:
                return 2;
            case JOpcode.cDUP_X1:
                return 3;
            case JOpcode.cWIDE: {
                int op = codeArray.getU1(pc + 1);
                if (op >= JOpcode.cILOAD && op <= JOpcode.cALOAD)
                    return 1;
                else if (op >= JOpcode.cISTORE && op <= JOpcode.cASTORE)
                    return 0;
                else
                    return 0; // (IINC)
            }
            default:
                throw new Error("JOpcode implementation error");
            }
        }
    }

    /**
     * Returns the number of data types taken from the stack by the current
     * instruction.
     * @return The number of data types taken from the stack by the current
     * instruction.
     */
//     public int getConsumedDataTypesNumber() {
//         if (opcode.getConsumedDataTypes() == JOpcode.UNKNOWN_TYPE) {
//             switch (opcode.code) {
//             case 87 : return 1; // POP
//             case 88 : return 2; // POP2
//             case 89 : return 1; // DUP
//             case 90 : return 2; // DUP_X1
//             case 91 : // DUP_X2
//             case 92 : // DUP2
//             case 93 : // DUP2_X1
//             case 94 : // DUP2_X2
//                 throw new UnsupportedOperationException("Opcode " + opcode.name
//                                                         + " has a stack-dependant"
//                                                         + " data types consumption");
//             case 95 : return 2; // SWAP
//             case 179 : return 1; // PUTSTATIC
//             case 181 : return 1; // PUTFIELD
//             case 182 : // INVOKEVIRTUAL
//             case 183 : // INVOKESPECIAL
//             case 185 : // INVOKEINTERFACE
//                 s = epool.getClassMethodRef(codeArray.getU2(pc + 1)).split(" ")[3];
//                 return ((JMethodType)JType.parseSignature(s)).argTypes.length + 1;
//             case 184 : // INVOKESTATIC
//                 s = epool.getClassMethodRef(codeArray.getU2(pc + 1)).split(" ")[3];
//                 return ((JMethodType)JType.parseSignature(s)).argTypes.length;
//             case 196 : // WIDE
//                 int op = codeArray.getU1(pc + 1);
//                 if (op >= 21 && op <= 25) return 0; // (xLOAD)
//                 else if (op >= 54 && op <= 58) // (xSTORE)
//                     return JOpcode.OPCODES[op].getConsumedDataTypes().length;
//                 else return 0; // (IINC)
//             case 197 : return codeArray.getU1(pc + 3); // MULTIANEWARRAY
//             default : throw new Error("JOpcode implementation error");
//             }
//         } else return opcode.getConsumedDataTypes().length;
//     }


    // Return the number between 0 and 3 which, if added to the given
    // value, would yield a multiple of 4.
    protected int[] padding = { 0, 3, 2, 1 };
    protected int pad4(int value) {
        return padding[value % 4];
    }
}
