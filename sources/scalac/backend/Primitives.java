/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Primitives.java,v 1.12 2002/11/19 12:01:11 paltherr Exp $
// $Id$

package scalac.backend;

import java.util.Map;
import java.util.HashMap;

import scalac.Global;
import scalac.symtab.Definitions;
import scalac.symtab.TypeTags;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolNameWriter;
import scalac.symtab.Type;
import scalac.util.Name;
import scalac.util.Names;
import scalac.util.Debug;

/**
 * Primitive functions.
 *
 * @author Michel Schinz, Philippe Altherr
 * @version 1.0
 */

public class Primitives {

    //########################################################################
    // Primitives constants

    private static final Name RUNTIME_N   = Name.fromString("RunTime");

    private static final Name ZARRAY_N    = Name.fromString("zarray");
    private static final Name BARRAY_N    = Name.fromString("barray");
    private static final Name SARRAY_N    = Name.fromString("sarray");
    private static final Name CARRAY_N    = Name.fromString("carray");
    private static final Name IARRAY_N    = Name.fromString("iarray");
    private static final Name LARRAY_N    = Name.fromString("larray");
    private static final Name FARRAY_N    = Name.fromString("farray");
    private static final Name DARRAY_N    = Name.fromString("darray");
    private static final Name OARRAY_N    = Name.fromString("oarray");

    private static final Name ZARRAY_LENGTH_N=Name.fromString("zarray_length");
    private static final Name BARRAY_LENGTH_N=Name.fromString("barray_length");
    private static final Name SARRAY_LENGTH_N=Name.fromString("sarray_length");
    private static final Name CARRAY_LENGTH_N=Name.fromString("carray_length");
    private static final Name IARRAY_LENGTH_N=Name.fromString("iarray_length");
    private static final Name LARRAY_LENGTH_N=Name.fromString("larray_length");
    private static final Name FARRAY_LENGTH_N=Name.fromString("farray_length");
    private static final Name DARRAY_LENGTH_N=Name.fromString("darray_length");
    private static final Name OARRAY_LENGTH_N=Name.fromString("oarray_length");

    private static final Name ZARRAY_GET_N = Name.fromString("zarray_get");
    private static final Name BARRAY_GET_N = Name.fromString("barray_get");
    private static final Name SARRAY_GET_N = Name.fromString("sarray_get");
    private static final Name CARRAY_GET_N = Name.fromString("carray_get");
    private static final Name IARRAY_GET_N = Name.fromString("iarray_get");
    private static final Name LARRAY_GET_N = Name.fromString("larray_get");
    private static final Name FARRAY_GET_N = Name.fromString("farray_get");
    private static final Name DARRAY_GET_N = Name.fromString("darray_get");
    private static final Name OARRAY_GET_N = Name.fromString("oarray_get");

    private static final Name ZARRAY_SET_N = Name.fromString("zarray_set");
    private static final Name BARRAY_SET_N = Name.fromString("barray_set");
    private static final Name SARRAY_SET_N = Name.fromString("sarray_set");
    private static final Name CARRAY_SET_N = Name.fromString("carray_set");
    private static final Name IARRAY_SET_N = Name.fromString("iarray_set");
    private static final Name LARRAY_SET_N = Name.fromString("larray_set");
    private static final Name FARRAY_SET_N = Name.fromString("farray_set");
    private static final Name DARRAY_SET_N = Name.fromString("darray_set");
    private static final Name OARRAY_SET_N = Name.fromString("oarray_set");

    private static final Name BOX_UVALUE_N = Name.fromString("box_uvalue");
    private static final Name BOX_ZVALUE_N = Name.fromString("box_zvalue");
    private static final Name BOX_BVALUE_N = Name.fromString("box_bvalue");
    private static final Name BOX_SVALUE_N = Name.fromString("box_svalue");
    private static final Name BOX_CVALUE_N = Name.fromString("box_cvalue");
    private static final Name BOX_IVALUE_N = Name.fromString("box_ivalue");
    private static final Name BOX_LVALUE_N = Name.fromString("box_lvalue");
    private static final Name BOX_FVALUE_N = Name.fromString("box_fvalue");
    private static final Name BOX_DVALUE_N = Name.fromString("box_dvalue");

    private static final Name BOX_ZARRAY_N = Name.fromString("box_zarray");
    private static final Name BOX_BARRAY_N = Name.fromString("box_barray");
    private static final Name BOX_SARRAY_N = Name.fromString("box_sarray");
    private static final Name BOX_CARRAY_N = Name.fromString("box_carray");
    private static final Name BOX_IARRAY_N = Name.fromString("box_iarray");
    private static final Name BOX_LARRAY_N = Name.fromString("box_larray");
    private static final Name BOX_FARRAY_N = Name.fromString("box_farray");
    private static final Name BOX_DARRAY_N = Name.fromString("box_darray");
    private static final Name BOX_OARRAY_N = Name.fromString("box_oarray");
    private static final Name BOX__ARRAY_N = Name.fromString("box__array");

    private static final Name UNBOX_UVALUE_N = Name.fromString("unbox_uvalue");
    private static final Name UNBOX_ZVALUE_N = Name.fromString("unbox_zvalue");
    private static final Name UNBOX_BVALUE_N = Name.fromString("unbox_bvalue");
    private static final Name UNBOX_SVALUE_N = Name.fromString("unbox_svalue");
    private static final Name UNBOX_CVALUE_N = Name.fromString("unbox_cvalue");
    private static final Name UNBOX_IVALUE_N = Name.fromString("unbox_ivalue");
    private static final Name UNBOX_LVALUE_N = Name.fromString("unbox_lvalue");
    private static final Name UNBOX_FVALUE_N = Name.fromString("unbox_fvalue");
    private static final Name UNBOX_DVALUE_N = Name.fromString("unbox_dvalue");

    private static final Name UNBOX_ZARRAY_N = Name.fromString("unbox_zarray");
    private static final Name UNBOX_BARRAY_N = Name.fromString("unbox_barray");
    private static final Name UNBOX_SARRAY_N = Name.fromString("unbox_sarray");
    private static final Name UNBOX_CARRAY_N = Name.fromString("unbox_carray");
    private static final Name UNBOX_IARRAY_N = Name.fromString("unbox_iarray");
    private static final Name UNBOX_LARRAY_N = Name.fromString("unbox_larray");
    private static final Name UNBOX_FARRAY_N = Name.fromString("unbox_farray");
    private static final Name UNBOX_DARRAY_N = Name.fromString("unbox_darray");
    private static final Name UNBOX_OARRAY_N = Name.fromString("unbox_oarray");
    private static final Name UNBOX__ARRAY_N = Name.fromString("unbox__array");

    private static final Name B2B_N = Name.fromString("b2b");
    private static final Name B2S_N = Name.fromString("b2s");
    private static final Name B2C_N = Name.fromString("b2c");
    private static final Name B2I_N = Name.fromString("b2i");
    private static final Name B2L_N = Name.fromString("b2l");
    private static final Name B2F_N = Name.fromString("b2f");
    private static final Name B2D_N = Name.fromString("b2d");
    private static final Name S2B_N = Name.fromString("s2b");
    private static final Name S2S_N = Name.fromString("s2s");
    private static final Name S2C_N = Name.fromString("s2c");
    private static final Name S2I_N = Name.fromString("s2i");
    private static final Name S2L_N = Name.fromString("s2l");
    private static final Name S2F_N = Name.fromString("s2f");
    private static final Name S2D_N = Name.fromString("s2d");
    private static final Name C2B_N = Name.fromString("c2b");
    private static final Name C2S_N = Name.fromString("c2s");
    private static final Name C2C_N = Name.fromString("c2c");
    private static final Name C2I_N = Name.fromString("c2i");
    private static final Name C2L_N = Name.fromString("c2l");
    private static final Name C2F_N = Name.fromString("c2f");
    private static final Name C2D_N = Name.fromString("c2d");
    private static final Name I2B_N = Name.fromString("i2b");
    private static final Name I2S_N = Name.fromString("i2s");
    private static final Name I2C_N = Name.fromString("i2c");
    private static final Name I2I_N = Name.fromString("i2i");
    private static final Name I2L_N = Name.fromString("i2l");
    private static final Name I2F_N = Name.fromString("i2f");
    private static final Name I2D_N = Name.fromString("i2d");
    private static final Name L2B_N = Name.fromString("l2b");
    private static final Name L2S_N = Name.fromString("l2s");
    private static final Name L2C_N = Name.fromString("l2c");
    private static final Name L2I_N = Name.fromString("l2i");
    private static final Name L2L_N = Name.fromString("l2l");
    private static final Name L2F_N = Name.fromString("l2f");
    private static final Name L2D_N = Name.fromString("l2d");
    private static final Name F2B_N = Name.fromString("f2b");
    private static final Name F2S_N = Name.fromString("f2s");
    private static final Name F2C_N = Name.fromString("f2c");
    private static final Name F2I_N = Name.fromString("f2i");
    private static final Name F2L_N = Name.fromString("f2l");
    private static final Name F2F_N = Name.fromString("f2f");
    private static final Name F2D_N = Name.fromString("f2d");
    private static final Name D2B_N = Name.fromString("d2b");
    private static final Name D2S_N = Name.fromString("d2s");
    private static final Name D2C_N = Name.fromString("d2c");
    private static final Name D2I_N = Name.fromString("d2i");
    private static final Name D2L_N = Name.fromString("d2l");
    private static final Name D2F_N = Name.fromString("d2f");
    private static final Name D2D_N = Name.fromString("d2d");

    //########################################################################
    // Primitives state

    private final Global global;
    private final Definitions definitions;
    private final Map/*<Symbol,Primitive>*/ primitives;
    private final SymbolNameWriter jreNameWriter;
    private final SymbolNameWriter clrNameWriter;

    public final Symbol RUNTIME;

    public final Symbol NEW_ZARRAY;
    public final Symbol NEW_BARRAY;
    public final Symbol NEW_SARRAY;
    public final Symbol NEW_CARRAY;
    public final Symbol NEW_IARRAY;
    public final Symbol NEW_LARRAY;
    public final Symbol NEW_FARRAY;
    public final Symbol NEW_DARRAY;
    public final Symbol NEW_OARRAY;

    public final Symbol ZARRAY_LENGTH;
    public final Symbol BARRAY_LENGTH;
    public final Symbol SARRAY_LENGTH;
    public final Symbol CARRAY_LENGTH;
    public final Symbol IARRAY_LENGTH;
    public final Symbol LARRAY_LENGTH;
    public final Symbol FARRAY_LENGTH;
    public final Symbol DARRAY_LENGTH;
    public final Symbol OARRAY_LENGTH;

    public final Symbol ZARRAY_GET;
    public final Symbol BARRAY_GET;
    public final Symbol SARRAY_GET;
    public final Symbol CARRAY_GET;
    public final Symbol IARRAY_GET;
    public final Symbol LARRAY_GET;
    public final Symbol FARRAY_GET;
    public final Symbol DARRAY_GET;
    public final Symbol OARRAY_GET;

    public final Symbol ZARRAY_SET;
    public final Symbol BARRAY_SET;
    public final Symbol SARRAY_SET;
    public final Symbol CARRAY_SET;
    public final Symbol IARRAY_SET;
    public final Symbol LARRAY_SET;
    public final Symbol FARRAY_SET;
    public final Symbol DARRAY_SET;
    public final Symbol OARRAY_SET;

    public final Symbol BOX_UVALUE;
    public final Symbol BOX_ZVALUE;
    public final Symbol BOX_BVALUE;
    public final Symbol BOX_SVALUE;
    public final Symbol BOX_CVALUE;
    public final Symbol BOX_IVALUE;
    public final Symbol BOX_LVALUE;
    public final Symbol BOX_FVALUE;
    public final Symbol BOX_DVALUE;

    public final Symbol BOX_ZARRAY;
    public final Symbol BOX_BARRAY;
    public final Symbol BOX_SARRAY;
    public final Symbol BOX_CARRAY;
    public final Symbol BOX_IARRAY;
    public final Symbol BOX_LARRAY;
    public final Symbol BOX_FARRAY;
    public final Symbol BOX_DARRAY;
    public final Symbol BOX_OARRAY;
    public final Symbol BOX__ARRAY;

    public final Symbol UNBOX_UVALUE;
    public final Symbol UNBOX_ZVALUE;
    public final Symbol UNBOX_BVALUE;
    public final Symbol UNBOX_SVALUE;
    public final Symbol UNBOX_CVALUE;
    public final Symbol UNBOX_IVALUE;
    public final Symbol UNBOX_LVALUE;
    public final Symbol UNBOX_FVALUE;
    public final Symbol UNBOX_DVALUE;

    public final Symbol UNBOX_ZARRAY;
    public final Symbol UNBOX_BARRAY;
    public final Symbol UNBOX_SARRAY;
    public final Symbol UNBOX_CARRAY;
    public final Symbol UNBOX_IARRAY;
    public final Symbol UNBOX_LARRAY;
    public final Symbol UNBOX_FARRAY;
    public final Symbol UNBOX_DARRAY;
    public final Symbol UNBOX_OARRAY;
    public final Symbol UNBOX__ARRAY;

    public final Symbol B2B;
    public final Symbol B2S;
    public final Symbol B2C;
    public final Symbol B2I;
    public final Symbol B2L;
    public final Symbol B2F;
    public final Symbol B2D;
    public final Symbol S2B;
    public final Symbol S2S;
    public final Symbol S2C;
    public final Symbol S2I;
    public final Symbol S2L;
    public final Symbol S2F;
    public final Symbol S2D;
    public final Symbol C2B;
    public final Symbol C2S;
    public final Symbol C2C;
    public final Symbol C2I;
    public final Symbol C2L;
    public final Symbol C2F;
    public final Symbol C2D;
    public final Symbol I2B;
    public final Symbol I2S;
    public final Symbol I2C;
    public final Symbol I2I;
    public final Symbol I2L;
    public final Symbol I2F;
    public final Symbol I2D;
    public final Symbol L2B;
    public final Symbol L2S;
    public final Symbol L2C;
    public final Symbol L2I;
    public final Symbol L2L;
    public final Symbol L2F;
    public final Symbol L2D;
    public final Symbol F2B;
    public final Symbol F2S;
    public final Symbol F2C;
    public final Symbol F2I;
    public final Symbol F2L;
    public final Symbol F2F;
    public final Symbol F2D;
    public final Symbol D2B;
    public final Symbol D2S;
    public final Symbol D2C;
    public final Symbol D2I;
    public final Symbol D2L;
    public final Symbol D2F;
    public final Symbol D2D;

    //########################################################################
    // Primitives constructor

    public Primitives(Global global) {
        this.global = global;
        this.definitions = global.definitions;
        this.primitives = new HashMap();
        this.jreNameWriter = new SymbolNameWriter().setClassSeparator('$');
        this.clrNameWriter = new SymbolNameWriter();
        this.RUNTIME = definitions.getModule("scala.runtime.RunTime");
        this.NEW_ZARRAY = getUniqueTerm(RUNTIME, ZARRAY_N);
        this.NEW_BARRAY = getUniqueTerm(RUNTIME, BARRAY_N);
        this.NEW_SARRAY = getUniqueTerm(RUNTIME, SARRAY_N);
        this.NEW_CARRAY = getUniqueTerm(RUNTIME, CARRAY_N);
        this.NEW_IARRAY = getUniqueTerm(RUNTIME, IARRAY_N);
        this.NEW_LARRAY = getUniqueTerm(RUNTIME, LARRAY_N);
        this.NEW_FARRAY = getUniqueTerm(RUNTIME, FARRAY_N);
        this.NEW_DARRAY = getUniqueTerm(RUNTIME, DARRAY_N);
        this.NEW_OARRAY = getUniqueTerm(RUNTIME, OARRAY_N);
        this.ZARRAY_LENGTH = getUniqueTerm(RUNTIME, ZARRAY_LENGTH_N);
        this.BARRAY_LENGTH = getUniqueTerm(RUNTIME, BARRAY_LENGTH_N);
        this.SARRAY_LENGTH = getUniqueTerm(RUNTIME, SARRAY_LENGTH_N);
        this.CARRAY_LENGTH = getUniqueTerm(RUNTIME, CARRAY_LENGTH_N);
        this.IARRAY_LENGTH = getUniqueTerm(RUNTIME, IARRAY_LENGTH_N);
        this.LARRAY_LENGTH = getUniqueTerm(RUNTIME, LARRAY_LENGTH_N);
        this.FARRAY_LENGTH = getUniqueTerm(RUNTIME, FARRAY_LENGTH_N);
        this.DARRAY_LENGTH = getUniqueTerm(RUNTIME, DARRAY_LENGTH_N);
        this.OARRAY_LENGTH = getUniqueTerm(RUNTIME, OARRAY_LENGTH_N);
        this.ZARRAY_GET = getUniqueTerm(RUNTIME, ZARRAY_GET_N);
        this.BARRAY_GET = getUniqueTerm(RUNTIME, BARRAY_GET_N);
        this.SARRAY_GET = getUniqueTerm(RUNTIME, SARRAY_GET_N);
        this.CARRAY_GET = getUniqueTerm(RUNTIME, CARRAY_GET_N);
        this.IARRAY_GET = getUniqueTerm(RUNTIME, IARRAY_GET_N);
        this.LARRAY_GET = getUniqueTerm(RUNTIME, LARRAY_GET_N);
        this.FARRAY_GET = getUniqueTerm(RUNTIME, FARRAY_GET_N);
        this.DARRAY_GET = getUniqueTerm(RUNTIME, DARRAY_GET_N);
        this.OARRAY_GET = getUniqueTerm(RUNTIME, OARRAY_GET_N);
        this.ZARRAY_SET = getUniqueTerm(RUNTIME, ZARRAY_SET_N);
        this.BARRAY_SET = getUniqueTerm(RUNTIME, BARRAY_SET_N);
        this.SARRAY_SET = getUniqueTerm(RUNTIME, SARRAY_SET_N);
        this.CARRAY_SET = getUniqueTerm(RUNTIME, CARRAY_SET_N);
        this.IARRAY_SET = getUniqueTerm(RUNTIME, IARRAY_SET_N);
        this.LARRAY_SET = getUniqueTerm(RUNTIME, LARRAY_SET_N);
        this.FARRAY_SET = getUniqueTerm(RUNTIME, FARRAY_SET_N);
        this.DARRAY_SET = getUniqueTerm(RUNTIME, DARRAY_SET_N);
        this.OARRAY_SET = getUniqueTerm(RUNTIME, OARRAY_SET_N);
        this.BOX_UVALUE = getUniqueTerm(RUNTIME, BOX_UVALUE_N);
        this.BOX_ZVALUE = getUniqueTerm(RUNTIME, BOX_ZVALUE_N);
        this.BOX_BVALUE = getUniqueTerm(RUNTIME, BOX_BVALUE_N);
        this.BOX_SVALUE = getUniqueTerm(RUNTIME, BOX_SVALUE_N);
        this.BOX_CVALUE = getUniqueTerm(RUNTIME, BOX_CVALUE_N);
        this.BOX_IVALUE = getUniqueTerm(RUNTIME, BOX_IVALUE_N);
        this.BOX_LVALUE = getUniqueTerm(RUNTIME, BOX_LVALUE_N);
        this.BOX_FVALUE = getUniqueTerm(RUNTIME, BOX_FVALUE_N);
        this.BOX_DVALUE = getUniqueTerm(RUNTIME, BOX_DVALUE_N);
        this.BOX_ZARRAY = getUniqueTerm(RUNTIME, BOX_ZARRAY_N);
        this.BOX_BARRAY = getUniqueTerm(RUNTIME, BOX_BARRAY_N);
        this.BOX_SARRAY = getUniqueTerm(RUNTIME, BOX_SARRAY_N);
        this.BOX_CARRAY = getUniqueTerm(RUNTIME, BOX_CARRAY_N);
        this.BOX_IARRAY = getUniqueTerm(RUNTIME, BOX_IARRAY_N);
        this.BOX_LARRAY = getUniqueTerm(RUNTIME, BOX_LARRAY_N);
        this.BOX_FARRAY = getUniqueTerm(RUNTIME, BOX_FARRAY_N);
        this.BOX_DARRAY = getUniqueTerm(RUNTIME, BOX_DARRAY_N);
        this.BOX_OARRAY = getUniqueTerm(RUNTIME, BOX_OARRAY_N);
        this.BOX__ARRAY = getUniqueTerm(RUNTIME, BOX__ARRAY_N);
        this.UNBOX_UVALUE = getUniqueTerm(RUNTIME, UNBOX_UVALUE_N);
        this.UNBOX_ZVALUE = getUniqueTerm(RUNTIME, UNBOX_ZVALUE_N);
        this.UNBOX_BVALUE = getUniqueTerm(RUNTIME, UNBOX_BVALUE_N);
        this.UNBOX_SVALUE = getUniqueTerm(RUNTIME, UNBOX_SVALUE_N);
        this.UNBOX_CVALUE = getUniqueTerm(RUNTIME, UNBOX_CVALUE_N);
        this.UNBOX_IVALUE = getUniqueTerm(RUNTIME, UNBOX_IVALUE_N);
        this.UNBOX_LVALUE = getUniqueTerm(RUNTIME, UNBOX_LVALUE_N);
        this.UNBOX_FVALUE = getUniqueTerm(RUNTIME, UNBOX_FVALUE_N);
        this.UNBOX_DVALUE = getUniqueTerm(RUNTIME, UNBOX_DVALUE_N);
        this.UNBOX_ZARRAY = getUniqueTerm(RUNTIME, UNBOX_ZARRAY_N);
        this.UNBOX_BARRAY = getUniqueTerm(RUNTIME, UNBOX_BARRAY_N);
        this.UNBOX_SARRAY = getUniqueTerm(RUNTIME, UNBOX_SARRAY_N);
        this.UNBOX_CARRAY = getUniqueTerm(RUNTIME, UNBOX_CARRAY_N);
        this.UNBOX_IARRAY = getUniqueTerm(RUNTIME, UNBOX_IARRAY_N);
        this.UNBOX_LARRAY = getUniqueTerm(RUNTIME, UNBOX_LARRAY_N);
        this.UNBOX_FARRAY = getUniqueTerm(RUNTIME, UNBOX_FARRAY_N);
        this.UNBOX_DARRAY = getUniqueTerm(RUNTIME, UNBOX_DARRAY_N);
        this.UNBOX_OARRAY = getUniqueTerm(RUNTIME, UNBOX_OARRAY_N);
        this.UNBOX__ARRAY = getUniqueTerm(RUNTIME, UNBOX__ARRAY_N);
        this.B2B = getUniqueTerm(RUNTIME, B2B_N);
        this.B2S = getUniqueTerm(RUNTIME, B2S_N);
        this.B2C = getUniqueTerm(RUNTIME, B2C_N);
        this.B2I = getUniqueTerm(RUNTIME, B2I_N);
        this.B2L = getUniqueTerm(RUNTIME, B2L_N);
        this.B2F = getUniqueTerm(RUNTIME, B2F_N);
        this.B2D = getUniqueTerm(RUNTIME, B2D_N);
        this.S2B = getUniqueTerm(RUNTIME, S2B_N);
        this.S2S = getUniqueTerm(RUNTIME, S2S_N);
        this.S2C = getUniqueTerm(RUNTIME, S2C_N);
        this.S2I = getUniqueTerm(RUNTIME, S2I_N);
        this.S2L = getUniqueTerm(RUNTIME, S2L_N);
        this.S2F = getUniqueTerm(RUNTIME, S2F_N);
        this.S2D = getUniqueTerm(RUNTIME, S2D_N);
        this.C2B = getUniqueTerm(RUNTIME, C2B_N);
        this.C2S = getUniqueTerm(RUNTIME, C2S_N);
        this.C2C = getUniqueTerm(RUNTIME, C2C_N);
        this.C2I = getUniqueTerm(RUNTIME, C2I_N);
        this.C2L = getUniqueTerm(RUNTIME, C2L_N);
        this.C2F = getUniqueTerm(RUNTIME, C2F_N);
        this.C2D = getUniqueTerm(RUNTIME, C2D_N);
        this.I2B = getUniqueTerm(RUNTIME, I2B_N);
        this.I2S = getUniqueTerm(RUNTIME, I2S_N);
        this.I2C = getUniqueTerm(RUNTIME, I2C_N);
        this.I2I = getUniqueTerm(RUNTIME, I2I_N);
        this.I2L = getUniqueTerm(RUNTIME, I2L_N);
        this.I2F = getUniqueTerm(RUNTIME, I2F_N);
        this.I2D = getUniqueTerm(RUNTIME, I2D_N);
        this.L2B = getUniqueTerm(RUNTIME, L2B_N);
        this.L2S = getUniqueTerm(RUNTIME, L2S_N);
        this.L2C = getUniqueTerm(RUNTIME, L2C_N);
        this.L2I = getUniqueTerm(RUNTIME, L2I_N);
        this.L2L = getUniqueTerm(RUNTIME, L2L_N);
        this.L2F = getUniqueTerm(RUNTIME, L2F_N);
        this.L2D = getUniqueTerm(RUNTIME, L2D_N);
        this.F2B = getUniqueTerm(RUNTIME, F2B_N);
        this.F2S = getUniqueTerm(RUNTIME, F2S_N);
        this.F2C = getUniqueTerm(RUNTIME, F2C_N);
        this.F2I = getUniqueTerm(RUNTIME, F2I_N);
        this.F2L = getUniqueTerm(RUNTIME, F2L_N);
        this.F2F = getUniqueTerm(RUNTIME, F2F_N);
        this.F2D = getUniqueTerm(RUNTIME, F2D_N);
        this.D2B = getUniqueTerm(RUNTIME, D2B_N);
        this.D2S = getUniqueTerm(RUNTIME, D2S_N);
        this.D2C = getUniqueTerm(RUNTIME, D2C_N);
        this.D2I = getUniqueTerm(RUNTIME, D2I_N);
        this.D2L = getUniqueTerm(RUNTIME, D2L_N);
        this.D2F = getUniqueTerm(RUNTIME, D2F_N);
        this.D2D = getUniqueTerm(RUNTIME, D2D_N);
        initPrimitives();
    }

    //########################################################################
    // Private interface

    private Symbol getUniqueTerm(Symbol owner, Name name) {
        Symbol symbol = getTerm(owner, name);
        assert !symbol.isOverloaded() :
            Debug.show(owner) + "." + name + " -> " + Debug.show(symbol);
        return symbol;
    }

    private Symbol getTerm(Symbol owner, Name name) {
        Symbol symbol = owner.lookup(name);
        assert symbol != Symbol.NONE : Debug.show(owner) + "." + name;
        return symbol;
    }

    private void initPrimitives() {
        Definitions defs = definitions;

        // scala.Any
        addPrimitive(defs.ANY_IS_ERASED, Primitive.IS);
        addPrimitive(defs.ANY_AS_ERASED, Primitive.AS);
        addPrimitive(defs.ANY_EQEQ, Primitive.EQ);
        addPrimitive(defs.ANY_BANGEQ, Primitive.NE);
        // !!! addPrimitive(defs.ANY_EQUALS, Primitive.EQUALS);
        addPrimitive(defs.ANY_HASHCODE, Primitive.HASHCODE);
        addPrimitive(defs.ANY_TOSTRING, Primitive.TOSTRING);

        // scala.Unit
        addAll(defs.UNIT_CLASS, Names.EQ, Primitive.EQ, 1);
        addAll(defs.UNIT_CLASS, Names.NE, Primitive.NE, 1);
        addAll(defs.UNIT_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.UNIT_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.UNIT_CLASS, Names.toString, Primitive.TOSTRING, 1);
        // !!! addAll(defs.UNIT_CLASS, Names.ADD, Primitive.CONCAT, 1);

        // scala.Boolean
        //addAll(defs.BOOLEAN_CLASS, Names.EQ, Primitive.EQ, 2);
        addAllPrimitive(defs.BOOLEAN_CLASS, Names.EQ, Primitive.EQ, 1);
        //addAll(defs.BOOLEAN_CLASS, Names.NE, Primitive.NE, 2);
        addAllPrimitive(defs.BOOLEAN_CLASS, Names.NE, Primitive.NE, 1);
        addAll(defs.BOOLEAN_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.BOOLEAN_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.BOOLEAN_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addAll(defs.BOOLEAN_CLASS, Names.ZNOT, Primitive.ZNOT, 1);
        addAll(defs.BOOLEAN_CLASS, Names.OR, Primitive.OR, 1);
        addAll(defs.BOOLEAN_CLASS, Names.XOR, Primitive.XOR, 1);
        addAll(defs.BOOLEAN_CLASS, Names.AND, Primitive.AND, 1);
        addAll(defs.BOOLEAN_CLASS, Names.ZOR, Primitive.ZOR, 1);
        addAll(defs.BOOLEAN_CLASS, Names.ZAND, Primitive.ZAND, 1);
        // !!! addAll(defs.BOOLEAN_CLASS, Names.ADD, Primitive.CONCAT, 1);

        // scala.Byte
        addAll(defs.BYTE_CLASS, Names.coerce, Primitive.COERCE, 5);
        //addAll(defs.BYTE_CLASS, Names.EQ, Primitive.EQ, 5);
        addAllPrimitive(defs.BYTE_CLASS, Names.EQ, Primitive.EQ, 7);
        //addAll(defs.BYTE_CLASS, Names.NE, Primitive.NE, 5);
        addAllPrimitive(defs.BYTE_CLASS, Names.NE, Primitive.NE, 7);
        //addAll(defs.BYTE_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.BYTE_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.BYTE_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addAll(defs.BYTE_CLASS, Names.NOT, Primitive.NOT, 1);
        addAdd(defs.BYTE_CLASS, 7);
        addSub(defs.BYTE_CLASS, 7);
        addAll(defs.BYTE_CLASS, Names.MUL, Primitive.MUL, 7);
        addAll(defs.BYTE_CLASS, Names.DIV, Primitive.DIV, 7);
        addAll(defs.BYTE_CLASS, Names.MOD, Primitive.MOD, 7);
        addAll(defs.BYTE_CLASS, Names.LT, Primitive.LT, 7);
        addAll(defs.BYTE_CLASS, Names.LE, Primitive.LE, 7);
        addAll(defs.BYTE_CLASS, Names.GT, Primitive.GT, 7);
        addAll(defs.BYTE_CLASS, Names.GE, Primitive.GE, 7);
        addAll(defs.BYTE_CLASS, Names.OR, Primitive.OR, 5);
        addAll(defs.BYTE_CLASS, Names.XOR, Primitive.XOR, 5);
        addAll(defs.BYTE_CLASS, Names.AND, Primitive.AND, 5);
        addAll(defs.BYTE_CLASS, Names.LSL, Primitive.LSL, 2);
        addAll(defs.BYTE_CLASS, Names.LSR, Primitive.LSR, 2);
        addAll(defs.BYTE_CLASS, Names.ASR, Primitive.ASR, 2);

        // scala.Short
        addAll(defs.SHORT_CLASS, Names.coerce, Primitive.COERCE, 4);
        //addAll(defs.SHORT_CLASS, Names.EQ, Primitive.EQ, 5);
        addAllPrimitive(defs.SHORT_CLASS, Names.EQ, Primitive.EQ, 7);
        //addAll(defs.SHORT_CLASS, Names.NE, Primitive.NE, 5);
        addAllPrimitive(defs.SHORT_CLASS, Names.NE, Primitive.NE, 7);
        //addAll(defs.SHORT_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.SHORT_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.SHORT_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addAll(defs.SHORT_CLASS, Names.NOT, Primitive.NOT, 1);
        addAdd(defs.SHORT_CLASS, 7);
        addSub(defs.SHORT_CLASS, 7);
        addAll(defs.SHORT_CLASS, Names.MUL, Primitive.MUL, 7);
        addAll(defs.SHORT_CLASS, Names.DIV, Primitive.DIV, 7);
        addAll(defs.SHORT_CLASS, Names.MOD, Primitive.MOD, 7);
        addAll(defs.SHORT_CLASS, Names.LT, Primitive.LT, 7);
        addAll(defs.SHORT_CLASS, Names.LE, Primitive.LE, 7);
        addAll(defs.SHORT_CLASS, Names.GT, Primitive.GT, 7);
        addAll(defs.SHORT_CLASS, Names.GE, Primitive.GE, 7);
        addAll(defs.SHORT_CLASS, Names.OR, Primitive.OR, 5);
        addAll(defs.SHORT_CLASS, Names.XOR, Primitive.XOR, 5);
        addAll(defs.SHORT_CLASS, Names.AND, Primitive.AND, 5);
        addAll(defs.SHORT_CLASS, Names.LSL, Primitive.LSL, 2);
        addAll(defs.SHORT_CLASS, Names.LSR, Primitive.LSR, 2);
        addAll(defs.SHORT_CLASS, Names.ASR, Primitive.ASR, 2);

        // scala.Char
        addAll(defs.CHAR_CLASS, Names.coerce, Primitive.COERCE, 4);
        //addAll(defs.CHAR_CLASS, Names.EQ, Primitive.EQ, 5);
        addAllPrimitive(defs.CHAR_CLASS, Names.EQ, Primitive.EQ, 7);
        //addAll(defs.CHAR_CLASS, Names.NE, Primitive.NE, 5);
        addAllPrimitive(defs.CHAR_CLASS, Names.NE, Primitive.NE, 7);
        //addAll(defs.CHAR_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.CHAR_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.CHAR_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addAll(defs.CHAR_CLASS, Names.NOT, Primitive.NOT, 1);
        addAdd(defs.CHAR_CLASS, 7);
        addSub(defs.CHAR_CLASS, 7);
        addAll(defs.CHAR_CLASS, Names.MUL, Primitive.MUL, 7);
        addAll(defs.CHAR_CLASS, Names.DIV, Primitive.DIV, 7);
        addAll(defs.CHAR_CLASS, Names.MOD, Primitive.MOD, 7);
        addAll(defs.CHAR_CLASS, Names.LT, Primitive.LT, 7);
        addAll(defs.CHAR_CLASS, Names.LE, Primitive.LE, 7);
        addAll(defs.CHAR_CLASS, Names.GT, Primitive.GT, 7);
        addAll(defs.CHAR_CLASS, Names.GE, Primitive.GE, 7);
        addAll(defs.CHAR_CLASS, Names.OR, Primitive.OR, 5);
        addAll(defs.CHAR_CLASS, Names.XOR, Primitive.XOR, 5);
        addAll(defs.CHAR_CLASS, Names.AND, Primitive.AND, 5);
        addAll(defs.CHAR_CLASS, Names.LSL, Primitive.LSL, 2);
        addAll(defs.CHAR_CLASS, Names.LSR, Primitive.LSR, 2);
        addAll(defs.CHAR_CLASS, Names.ASR, Primitive.ASR, 2);

        // scala.Int
        addAll(defs.INT_CLASS, Names.coerce, Primitive.COERCE, 3);
        //addAll(defs.INT_CLASS, Names.EQ, Primitive.EQ, 5);
        addAllPrimitive(defs.INT_CLASS, Names.EQ, Primitive.EQ, 7);
        //addAll(defs.INT_CLASS, Names.NE, Primitive.NE, 5);
        addAllPrimitive(defs.INT_CLASS, Names.NE, Primitive.NE, 7);
        //addAll(defs.INT_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.INT_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.INT_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addAll(defs.INT_CLASS, Names.NOT, Primitive.NOT, 1);
        addAdd(defs.INT_CLASS, 7);
        addSub(defs.INT_CLASS, 7);
        addAll(defs.INT_CLASS, Names.MUL, Primitive.MUL, 7);
        addAll(defs.INT_CLASS, Names.DIV, Primitive.DIV, 7);
        addAll(defs.INT_CLASS, Names.MOD, Primitive.MOD, 7);
        addAll(defs.INT_CLASS, Names.LT, Primitive.LT, 7);
        addAll(defs.INT_CLASS, Names.LE, Primitive.LE, 7);
        addAll(defs.INT_CLASS, Names.GT, Primitive.GT, 7);
        addAll(defs.INT_CLASS, Names.GE, Primitive.GE, 7);
        addAll(defs.INT_CLASS, Names.OR, Primitive.OR, 5);
        addAll(defs.INT_CLASS, Names.XOR, Primitive.XOR, 5);
        addAll(defs.INT_CLASS, Names.AND, Primitive.AND, 5);
        addAll(defs.INT_CLASS, Names.LSL, Primitive.LSL, 2);
        addAll(defs.INT_CLASS, Names.LSR, Primitive.LSR, 2);
        addAll(defs.INT_CLASS, Names.ASR, Primitive.ASR, 2);

        // scala.Long
        addAll(defs.LONG_CLASS, Names.coerce, Primitive.COERCE, 2);
        //addAll(defs.LONG_CLASS, Names.EQ, Primitive.EQ, 4);
        addAllPrimitive(defs.LONG_CLASS, Names.EQ, Primitive.EQ, 7);
        //addAll(defs.LONG_CLASS, Names.NE, Primitive.NE, 4);
        addAllPrimitive(defs.LONG_CLASS, Names.NE, Primitive.NE, 7);
        //addAll(defs.LONG_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.LONG_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.LONG_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addAll(defs.LONG_CLASS, Names.NOT, Primitive.NOT, 1);
        addAdd(defs.LONG_CLASS, 7);
        addSub(defs.LONG_CLASS, 7);
        addAll(defs.LONG_CLASS, Names.MUL, Primitive.MUL, 7);
        addAll(defs.LONG_CLASS, Names.DIV, Primitive.DIV, 7);
        addAll(defs.LONG_CLASS, Names.MOD, Primitive.MOD, 7);
        addAll(defs.LONG_CLASS, Names.LT, Primitive.LT, 7);
        addAll(defs.LONG_CLASS, Names.LE, Primitive.LE, 7);
        addAll(defs.LONG_CLASS, Names.GT, Primitive.GT, 7);
        addAll(defs.LONG_CLASS, Names.GE, Primitive.GE, 7);
        addAll(defs.LONG_CLASS, Names.OR, Primitive.OR, 5);
        addAll(defs.LONG_CLASS, Names.XOR, Primitive.XOR, 5);
        addAll(defs.LONG_CLASS, Names.AND, Primitive.AND, 5);
        addAll(defs.LONG_CLASS, Names.LSL, Primitive.LSL, 2);
        addAll(defs.LONG_CLASS, Names.LSR, Primitive.LSR, 2);
        addAll(defs.LONG_CLASS, Names.ASR, Primitive.ASR, 2);

        // scala.Float
        addAll(defs.FLOAT_CLASS, Names.coerce, Primitive.COERCE, 1);
        //addAll(defs.FLOAT_CLASS, Names.EQ, Primitive.EQ, 3);
        addAllPrimitive(defs.FLOAT_CLASS, Names.EQ, Primitive.EQ, 2);
        //addAll(defs.FLOAT_CLASS, Names.NE, Primitive.NE, 3);
        addAllPrimitive(defs.FLOAT_CLASS, Names.NE, Primitive.NE, 2);
        //addAll(defs.FLOAT_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.FLOAT_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.FLOAT_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addAdd(defs.FLOAT_CLASS, 2);
        addSub(defs.FLOAT_CLASS, 2);
        addAll(defs.FLOAT_CLASS, Names.MUL, Primitive.MUL, 2);
        addAll(defs.FLOAT_CLASS, Names.DIV, Primitive.DIV, 2);
        addAll(defs.FLOAT_CLASS, Names.MOD, Primitive.MOD, 2);
        addAll(defs.FLOAT_CLASS, Names.LT, Primitive.LT, 2);
        addAll(defs.FLOAT_CLASS, Names.LE, Primitive.LE, 2);
        addAll(defs.FLOAT_CLASS, Names.GT, Primitive.GT, 2);
        addAll(defs.FLOAT_CLASS, Names.GE, Primitive.GE, 2);

        // scala.Double
        //addAll(defs.DOUBLE_CLASS, Names.EQ, Primitive.EQ, 2);
        addAllPrimitive(defs.DOUBLE_CLASS, Names.EQ, Primitive.EQ, 1);
        //addAll(defs.DOUBLE_CLASS, Names.NE, Primitive.NE, 2);
        addAllPrimitive(defs.DOUBLE_CLASS, Names.NE, Primitive.NE, 1);
        //addAll(defs.DOUBLE_CLASS, Names.equals, Primitive.EQUALS, 1);
        addAll(defs.DOUBLE_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        addAll(defs.DOUBLE_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addAdd(defs.DOUBLE_CLASS, 1);
        addSub(defs.DOUBLE_CLASS, 1);
        addAll(defs.DOUBLE_CLASS, Names.MUL, Primitive.MUL, 1);
        addAll(defs.DOUBLE_CLASS, Names.DIV, Primitive.DIV, 1);
        addAll(defs.DOUBLE_CLASS, Names.MOD, Primitive.MOD, 1);
        addAll(defs.DOUBLE_CLASS, Names.LT, Primitive.LT, 1);
        addAll(defs.DOUBLE_CLASS, Names.LE, Primitive.LE, 1);
        addAll(defs.DOUBLE_CLASS, Names.GT, Primitive.GT, 1);
        addAll(defs.DOUBLE_CLASS, Names.GE, Primitive.GE, 1);

        // scala.Object
        addPrimitive(defs.OBJECT_EQ, Primitive.ID);
        addPrimitive(defs.OBJECT_NE, Primitive.NI);
        addPrimitive(defs.OBJECT_SYNCHRONIZED, Primitive.SYNCHRONIZED);

        // scala.String
        addPrimitive(defs.STRING_PLUS, Primitive.CONCAT);

        // scala.Throwable
        addPrimitive(defs.THROWABLE_THROW, Primitive.THROW);

        // scala.Array
        // !!! addAll(defs.ARRAY_CLASS, defs.EQEQ_N, Primitive.EQ, 1);
        // !!! addAll(defs.ARRAY_CLASS, defs.BANGEQ_N, Primitive.NE, 1);
        // !!! addAll(defs.ARRAY_CLASS, Names.equals, Primitive.EQUALS, 1);
        // !!! addAll(defs.ARRAY_CLASS, Names.hashCode, Primitive.HASHCODE, 1);
        // !!! addAll(defs.ARRAY_CLASS, Names.toString, Primitive.TOSTRING, 1);
        addAll(defs.ARRAY_CLASS, Names.length, Primitive.LENGTH, 1);
        addAll(defs.ARRAY_CLASS, Names.apply, Primitive.APPLY, 1);
        addAll(defs.ARRAY_CLASS, Names.update, Primitive.UPDATE, 1);

        // scala.runtime.RunTime
        addPrimitive(BOX_UVALUE, Primitive.BOX);
        addPrimitive(BOX_ZVALUE, Primitive.BOX);
        addPrimitive(BOX_BVALUE, Primitive.BOX);
        addPrimitive(BOX_SVALUE, Primitive.BOX);
        addPrimitive(BOX_CVALUE, Primitive.BOX);
        addPrimitive(BOX_IVALUE, Primitive.BOX);
        addPrimitive(BOX_LVALUE, Primitive.BOX);
        addPrimitive(BOX_FVALUE, Primitive.BOX);
        addPrimitive(BOX_DVALUE, Primitive.BOX);
        addPrimitive(BOX_ZARRAY, Primitive.BOX);
        addPrimitive(BOX_BARRAY, Primitive.BOX);
        addPrimitive(BOX_SARRAY, Primitive.BOX);
        addPrimitive(BOX_CARRAY, Primitive.BOX);
        addPrimitive(BOX_IARRAY, Primitive.BOX);
        addPrimitive(BOX_LARRAY, Primitive.BOX);
        addPrimitive(BOX_FARRAY, Primitive.BOX);
        addPrimitive(BOX_DARRAY, Primitive.BOX);
        addPrimitive(BOX_OARRAY, Primitive.BOX);
        addPrimitive(BOX__ARRAY, Primitive.BOX);
        addPrimitive(UNBOX_UVALUE, Primitive.UNBOX);
        addPrimitive(UNBOX_ZVALUE, Primitive.UNBOX);
        addPrimitive(UNBOX_BVALUE, Primitive.UNBOX);
        addPrimitive(UNBOX_SVALUE, Primitive.UNBOX);
        addPrimitive(UNBOX_CVALUE, Primitive.UNBOX);
        addPrimitive(UNBOX_IVALUE, Primitive.UNBOX);
        addPrimitive(UNBOX_LVALUE, Primitive.UNBOX);
        addPrimitive(UNBOX_FVALUE, Primitive.UNBOX);
        addPrimitive(UNBOX_DVALUE, Primitive.UNBOX);
        addPrimitive(UNBOX_ZARRAY, Primitive.UNBOX);
        addPrimitive(UNBOX_BARRAY, Primitive.UNBOX);
        addPrimitive(UNBOX_SARRAY, Primitive.UNBOX);
        addPrimitive(UNBOX_CARRAY, Primitive.UNBOX);
        addPrimitive(UNBOX_IARRAY, Primitive.UNBOX);
        addPrimitive(UNBOX_LARRAY, Primitive.UNBOX);
        addPrimitive(UNBOX_FARRAY, Primitive.UNBOX);
        addPrimitive(UNBOX_DARRAY, Primitive.UNBOX);
        addPrimitive(UNBOX_OARRAY, Primitive.UNBOX);
        addPrimitive(UNBOX__ARRAY, Primitive.UNBOX);
        addPrimitive(B2B, Primitive.B2B);
        addPrimitive(B2S, Primitive.B2S);
        addPrimitive(B2C, Primitive.B2C);
        addPrimitive(B2I, Primitive.B2I);
        addPrimitive(B2L, Primitive.B2L);
        addPrimitive(B2F, Primitive.B2F);
        addPrimitive(B2D, Primitive.B2D);
        addPrimitive(S2B, Primitive.S2B);
        addPrimitive(S2S, Primitive.S2S);
        addPrimitive(S2C, Primitive.S2C);
        addPrimitive(S2I, Primitive.S2I);
        addPrimitive(S2L, Primitive.S2L);
        addPrimitive(S2F, Primitive.S2F);
        addPrimitive(S2D, Primitive.S2D);
        addPrimitive(C2B, Primitive.C2B);
        addPrimitive(C2S, Primitive.C2S);
        addPrimitive(C2C, Primitive.C2C);
        addPrimitive(C2I, Primitive.C2I);
        addPrimitive(C2L, Primitive.C2L);
        addPrimitive(C2F, Primitive.C2F);
        addPrimitive(C2D, Primitive.C2D);
        addPrimitive(I2B, Primitive.I2B);
        addPrimitive(I2S, Primitive.I2S);
        addPrimitive(I2C, Primitive.I2C);
        addPrimitive(I2I, Primitive.I2I);
        addPrimitive(I2L, Primitive.I2L);
        addPrimitive(I2F, Primitive.I2F);
        addPrimitive(I2D, Primitive.I2D);
        addPrimitive(L2B, Primitive.L2B);
        addPrimitive(L2S, Primitive.L2S);
        addPrimitive(L2C, Primitive.L2C);
        addPrimitive(L2I, Primitive.L2I);
        addPrimitive(L2L, Primitive.L2L);
        addPrimitive(L2F, Primitive.L2F);
        addPrimitive(L2D, Primitive.L2D);
        addPrimitive(F2B, Primitive.F2B);
        addPrimitive(F2S, Primitive.F2S);
        addPrimitive(F2C, Primitive.F2C);
        addPrimitive(F2I, Primitive.F2I);
        addPrimitive(F2L, Primitive.F2L);
        addPrimitive(F2F, Primitive.F2F);
        addPrimitive(F2D, Primitive.F2D);
        addPrimitive(D2B, Primitive.D2B);
        addPrimitive(D2S, Primitive.D2S);
        addPrimitive(D2C, Primitive.D2C);
        addPrimitive(D2I, Primitive.D2I);
        addPrimitive(D2L, Primitive.D2L);
        addPrimitive(D2F, Primitive.D2F);
        addPrimitive(D2D, Primitive.D2D);
        addPrimitive(NEW_ZARRAY, Primitive.NEW_ZARRAY);
        addPrimitive(NEW_BARRAY, Primitive.NEW_BARRAY);
        addPrimitive(NEW_SARRAY, Primitive.NEW_SARRAY);
        addPrimitive(NEW_CARRAY, Primitive.NEW_CARRAY);
        addPrimitive(NEW_IARRAY, Primitive.NEW_IARRAY);
        addPrimitive(NEW_LARRAY, Primitive.NEW_LARRAY);
        addPrimitive(NEW_FARRAY, Primitive.NEW_FARRAY);
        addPrimitive(NEW_DARRAY, Primitive.NEW_DARRAY);
        addPrimitive(NEW_OARRAY, Primitive.NEW_OARRAY);
        addPrimitive(ZARRAY_LENGTH, Primitive.ZARRAY_LENGTH);
        addPrimitive(BARRAY_LENGTH, Primitive.BARRAY_LENGTH);
        addPrimitive(SARRAY_LENGTH, Primitive.SARRAY_LENGTH);
        addPrimitive(CARRAY_LENGTH, Primitive.CARRAY_LENGTH);
        addPrimitive(IARRAY_LENGTH, Primitive.IARRAY_LENGTH);
        addPrimitive(LARRAY_LENGTH, Primitive.LARRAY_LENGTH);
        addPrimitive(FARRAY_LENGTH, Primitive.FARRAY_LENGTH);
        addPrimitive(DARRAY_LENGTH, Primitive.DARRAY_LENGTH);
        addPrimitive(OARRAY_LENGTH, Primitive.OARRAY_LENGTH);
        addPrimitive(ZARRAY_GET, Primitive.ZARRAY_GET);
        addPrimitive(BARRAY_GET, Primitive.BARRAY_GET);
        addPrimitive(SARRAY_GET, Primitive.SARRAY_GET);
        addPrimitive(CARRAY_GET, Primitive.CARRAY_GET);
        addPrimitive(IARRAY_GET, Primitive.IARRAY_GET);
        addPrimitive(LARRAY_GET, Primitive.LARRAY_GET);
        addPrimitive(FARRAY_GET, Primitive.FARRAY_GET);
        addPrimitive(DARRAY_GET, Primitive.DARRAY_GET);
        addPrimitive(OARRAY_GET, Primitive.OARRAY_GET);
        addPrimitive(ZARRAY_SET, Primitive.ZARRAY_SET);
        addPrimitive(BARRAY_SET, Primitive.BARRAY_SET);
        addPrimitive(SARRAY_SET, Primitive.SARRAY_SET);
        addPrimitive(CARRAY_SET, Primitive.CARRAY_SET);
        addPrimitive(IARRAY_SET, Primitive.IARRAY_SET);
        addPrimitive(LARRAY_SET, Primitive.LARRAY_SET);
        addPrimitive(FARRAY_SET, Primitive.FARRAY_SET);
        addPrimitive(DARRAY_SET, Primitive.DARRAY_SET);
        addPrimitive(OARRAY_SET, Primitive.OARRAY_SET);
    }

    private void addAdd(Symbol clasz, int count) {
        Name name = Names.ADD;
        Symbol symbol = clasz.lookup(name);
        assert !symbol.isNone(): Debug.show(clasz) + "." + name;
        Symbol[] alts = symbol.alternativeSymbols();
        boolean unary = false;
        boolean concat = false;
        for (int i = 0; i < alts.length; i++) {
            switch (alts[i].type()) {
            case MethodType(Symbol[] vparams, _):
                assert vparams.length == 1: alts[i].type();
                if (vparams[0].type().isSameAs(definitions.STRING_TYPE())) {
                    addPrimitive(alts[i], Primitive.CONCAT);
                    assert !concat;
                    concat = true;
                } else {
                    addPrimitive(alts[i], Primitive.ADD);
                    count--;
                }
                break;
            case PolyType(Symbol[] tparams, _):
                addPrimitive(alts[i], Primitive.POS);
                assert !unary;
                unary = true;
                break;
            default:
                throw Debug.abort("illegal case" , alts[i].type());
            }
        }
        assert count == 0 && unary && concat: count+" - "+unary+" - "+concat;
    }

    private void addSub(Symbol clasz, int count) {
        Name name = Names.SUB;
        Symbol symbol = clasz.lookup(name);
        assert !symbol.isNone(): Debug.show(clasz) + "." + name;
        Symbol[] alts = symbol.alternativeSymbols();
        boolean unary = false;
        for (int i = 0; i < alts.length; i++) {
            switch (alts[i].type()) {
            case MethodType(_, _):
                addPrimitive(alts[i], Primitive.SUB);
                count--;
                break;
            case PolyType(_, _):
                addPrimitive(alts[i], Primitive.NEG);
                assert !unary;
                unary = true;
                break;
            default:
                throw Debug.abort("illegal case" , alts[i].type());
            }
        }
        assert count == 0 && unary: count + " - " + unary;
    }

    private void addAll(Symbol clasz,Name name,Primitive primitive,int count) {
        Symbol symbol = clasz.lookup(name);
        assert symbol != Symbol.NONE : Debug.show(clasz) + "." + name;
        Symbol[] alts = symbol.alternativeSymbols();
        assert count == alts.length : count + " != " + alts.length;
        for (int i = 0; i < alts.length; i++) addPrimitive(alts[i], primitive);
    }

    private void addAllPrimitive(Symbol clasz, Name name, Primitive primitive, int count) {
        Symbol symbol = clasz.lookup(name);
        assert symbol != Symbol.NONE : Debug.show(clasz) + "." + name;
        Symbol[] alts = symbol.alternativeSymbols();
        int cnt = 0;
        loop:
        for (int i = 0; i < alts.length; i++) {
            switch (alts[i].info()) {
            case MethodType(Symbol[] vparams, _):
                for (int j = 0; j < vparams.length; j++) {
                    if (!isValueType(vparams[j].info()))
                        continue loop;
                }
                addPrimitive(alts[i], primitive);
                cnt++;
                break;
            }
        }
        assert cnt == count : "" + cnt + " != " + count;
    }

    private boolean isValueType(Type t) {
        return t.isSubType(definitions.ANYVAL_TYPE());
    }

    private void addPrimitive(Symbol symbol, Primitive primitive) {
        assert !primitives.containsKey(symbol) : Debug.show(symbol);
        primitives.put(symbol, primitive);
    }

    //########################################################################
    // Primitives interface - general primitives

    /** Return true iff the given symbol refers to a primitive. */
    public boolean isPrimitive(Symbol symbol) {
        return primitives.containsKey(symbol);
    }

    /** Return primitive identified by the symbol. */
    public Primitive getPrimitive(Symbol symbol) {
        Object value = primitives.get(symbol);
        return value == null ? Primitive.NOT_A_PRIMITIVE : (Primitive)value;
    }

    //########################################################################
    // Primitives interface - array creation primitives

    /** Return array creation method for elements of the given kind. */
    public Symbol getNewArraySymbol(int kind) {
        switch (kind) {
        case TypeTags.BOOLEAN: return NEW_ZARRAY;
        case TypeTags.BYTE   : return NEW_BARRAY;
        case TypeTags.SHORT  : return NEW_SARRAY;
        case TypeTags.CHAR   : return NEW_CARRAY;
        case TypeTags.INT    : return NEW_IARRAY;
        case TypeTags.LONG   : return NEW_LARRAY;
        case TypeTags.FLOAT  : return NEW_FARRAY;
        case TypeTags.DOUBLE : return NEW_DARRAY;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    //########################################################################
    // Primitives interface - boxing primitives

    /** Return box method for values of the given type. */
    public Symbol getBoxValueSymbol(Type type) {
        switch (type) {
        case UnboxedType(int kind):
            return getBoxValueSymbol(kind);
        case UnboxedArrayType(Type elemtp):
            return getBoxArraySymbol(elemtp);
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    /** Return box method for values of the given kind. */
    public Symbol getBoxValueSymbol(int kind) {
        switch (kind) {
        case TypeTags.UNIT   : return BOX_UVALUE;
        case TypeTags.BOOLEAN: return BOX_ZVALUE;
        case TypeTags.BYTE   : return BOX_BVALUE;
        case TypeTags.SHORT  : return BOX_SVALUE;
        case TypeTags.CHAR   : return BOX_CVALUE;
        case TypeTags.INT    : return BOX_IVALUE;
        case TypeTags.LONG   : return BOX_LVALUE;
        case TypeTags.FLOAT  : return BOX_FVALUE;
        case TypeTags.DOUBLE : return BOX_DVALUE;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    /** Return box method for arrays of elements of the given type. */
    public Symbol getBoxArraySymbol(Type type) {
        switch (type) {
        case UnboxedType(int kind):
            return getBoxArraySymbol(kind);
        default:
            return BOX_OARRAY;
        }
    }

    /** Return box method for arrays of elements of the given kind. */
    public Symbol getBoxArraySymbol(int kind) {
        switch (kind) {
        case TypeTags.BOOLEAN: return BOX_ZARRAY;
        case TypeTags.BYTE   : return BOX_BARRAY;
        case TypeTags.SHORT  : return BOX_SARRAY;
        case TypeTags.CHAR   : return BOX_CARRAY;
        case TypeTags.INT    : return BOX_IARRAY;
        case TypeTags.LONG   : return BOX_LARRAY;
        case TypeTags.FLOAT  : return BOX_FARRAY;
        case TypeTags.DOUBLE : return BOX_DARRAY;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    //########################################################################
    // Primitives interface - unboxing primitives

    /** Return unbox method returning values of the given type. */
    public Symbol getUnboxValueSymbol(Type type) {
        switch (type) {
        case UnboxedType(int kind):
            return getUnboxValueSymbol(kind);
        case UnboxedArrayType(Type elemtp):
            return getUnboxArraySymbol(elemtp);
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    /** Return unbox method returning values of the given kind. */
    public Symbol getUnboxValueSymbol(int kind) {
        switch (kind) {
        case TypeTags.UNIT   : return UNBOX_UVALUE;
        case TypeTags.BOOLEAN: return UNBOX_ZVALUE;
        case TypeTags.BYTE   : return UNBOX_BVALUE;
        case TypeTags.SHORT  : return UNBOX_SVALUE;
        case TypeTags.CHAR   : return UNBOX_CVALUE;
        case TypeTags.INT    : return UNBOX_IVALUE;
        case TypeTags.LONG   : return UNBOX_LVALUE;
        case TypeTags.FLOAT  : return UNBOX_FVALUE;
        case TypeTags.DOUBLE : return UNBOX_DVALUE;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    /** Return unbox method returning arrays of elements of the given type. */
    public Symbol getUnboxArraySymbol(Type type) {
        switch (type) {
        case UnboxedType(int kind):
            return getUnboxArraySymbol(kind);
        default:
            return UNBOX_OARRAY;
        }
    }

    /** Return unbox method returning arrays of elements of the given kind. */
    public Symbol getUnboxArraySymbol(int kind) {
        switch (kind) {
        case TypeTags.BOOLEAN: return UNBOX_ZARRAY;
        case TypeTags.BYTE   : return UNBOX_BARRAY;
        case TypeTags.SHORT  : return UNBOX_SARRAY;
        case TypeTags.CHAR   : return UNBOX_CARRAY;
        case TypeTags.INT    : return UNBOX_IARRAY;
        case TypeTags.LONG   : return UNBOX_LARRAY;
        case TypeTags.FLOAT  : return UNBOX_FARRAY;
        case TypeTags.DOUBLE : return UNBOX_DARRAY;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    //########################################################################
    // Primitives interface - conversion primitives

    /** Return conversion method for given types. */
    public Symbol getConvertSymbol(Type from, Type to) {
        switch (from) {
        case UnboxedType(int kind):
            return getConvertSymbol(kind, to);
        default:
            throw Debug.abort("illegal case", from);
        }
    }

    /** Return conversion method for given type and type kind. */
    public Symbol getConvertSymbol(Type from, int to) {
        switch (from) {
        case UnboxedType(int kind):
            return getConvertSymbol(kind, to);
        default:
            throw Debug.abort("illegal case", from);
        }
    }

    /** Return conversion method for given type kind and type. */
    public Symbol getConvertSymbol(int from, Type to) {
        switch (to) {
        case UnboxedType(int kind):
            return getConvertSymbol(from, kind);
        default:
            throw Debug.abort("illegal case", to);
        }
    }

    /** Return conversion method for given kind types. */
    public Symbol getConvertSymbol(int from, int to) {
        switch (from) {
        case TypeTags.BYTE   :
            switch (to) {
            case TypeTags.BYTE   : return B2B;
            case TypeTags.SHORT  : return B2S;
            case TypeTags.CHAR   : return B2C;
            case TypeTags.INT    : return B2I;
            case TypeTags.LONG   : return B2L;
            case TypeTags.FLOAT  : return B2F;
            case TypeTags.DOUBLE : return B2D;
            } break;
        case TypeTags.SHORT  :
            switch (to) {
            case TypeTags.BYTE   : return S2B;
            case TypeTags.SHORT  : return S2S;
            case TypeTags.CHAR   : return S2C;
            case TypeTags.INT    : return S2I;
            case TypeTags.LONG   : return S2L;
            case TypeTags.FLOAT  : return S2F;
            case TypeTags.DOUBLE : return S2D;
            } break;
        case TypeTags.CHAR   :
            switch (to) {
            case TypeTags.BYTE   : return C2B;
            case TypeTags.SHORT  : return C2S;
            case TypeTags.CHAR   : return C2C;
            case TypeTags.INT    : return C2I;
            case TypeTags.LONG   : return C2L;
            case TypeTags.FLOAT  : return C2F;
            case TypeTags.DOUBLE : return C2D;
            } break;
        case TypeTags.INT    :
            switch (to) {
            case TypeTags.BYTE   : return I2B;
            case TypeTags.SHORT  : return I2S;
            case TypeTags.CHAR   : return I2C;
            case TypeTags.INT    : return I2I;
            case TypeTags.LONG   : return I2L;
            case TypeTags.FLOAT  : return I2F;
            case TypeTags.DOUBLE : return I2D;
            } break;
        case TypeTags.LONG   :
            switch (to) {
            case TypeTags.BYTE   : return L2B;
            case TypeTags.SHORT  : return L2S;
            case TypeTags.CHAR   : return L2C;
            case TypeTags.INT    : return L2I;
            case TypeTags.LONG   : return L2L;
            case TypeTags.FLOAT  : return L2F;
            case TypeTags.DOUBLE : return L2D;
            } break;
        case TypeTags.FLOAT  :
            switch (to) {
            case TypeTags.BYTE   : return F2B;
            case TypeTags.SHORT  : return F2S;
            case TypeTags.CHAR   : return F2C;
            case TypeTags.INT    : return F2I;
            case TypeTags.LONG   : return F2L;
            case TypeTags.FLOAT  : return F2F;
            case TypeTags.DOUBLE : return F2D;
            } break;
        case TypeTags.DOUBLE :
            switch (to) {
            case TypeTags.BYTE   : return D2B;
            case TypeTags.SHORT  : return D2S;
            case TypeTags.CHAR   : return D2C;
            case TypeTags.INT    : return D2I;
            case TypeTags.LONG   : return D2L;
            case TypeTags.FLOAT  : return D2F;
            case TypeTags.DOUBLE : return D2D;
            } break;
        }
        throw Debug.abort("illegal case: " + from + " -> " + to);
    }

    //########################################################################
    // Primitives interface - array get and set primitives

    /** Return length method for arrays of the given type. */
    public Symbol getArrayLengthSymbol(Type type) {
        switch (type) {
        case UnboxedArrayType(UnboxedType(int kind)):
            return getArrayLengthSymbol(kind);
        case UnboxedArrayType(_):
            return OARRAY_LENGTH;
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    /** Return length method for arrays of elements of the given kind. */
    public Symbol getArrayLengthSymbol(int kind) {
        switch (kind) {
        case TypeTags.BOOLEAN: return ZARRAY_LENGTH;
        case TypeTags.BYTE   : return BARRAY_LENGTH;
        case TypeTags.SHORT  : return SARRAY_LENGTH;
        case TypeTags.CHAR   : return CARRAY_LENGTH;
        case TypeTags.INT    : return IARRAY_LENGTH;
        case TypeTags.LONG   : return LARRAY_LENGTH;
        case TypeTags.FLOAT  : return FARRAY_LENGTH;
        case TypeTags.DOUBLE : return DARRAY_LENGTH;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    /** Return get method for arrays of the given type. */
    public Symbol getArrayGetSymbol(Type type) {
        switch (type) {
        case UnboxedArrayType(UnboxedType(int kind)):
            return getArrayGetSymbol(kind);
        case UnboxedArrayType(_):
            return OARRAY_GET;
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    /** Return get method for arrays of elements of the given kind. */
    public Symbol getArrayGetSymbol(int kind) {
        switch (kind) {
        case TypeTags.BOOLEAN: return ZARRAY_GET;
        case TypeTags.BYTE   : return BARRAY_GET;
        case TypeTags.SHORT  : return SARRAY_GET;
        case TypeTags.CHAR   : return CARRAY_GET;
        case TypeTags.INT    : return IARRAY_GET;
        case TypeTags.LONG   : return LARRAY_GET;
        case TypeTags.FLOAT  : return FARRAY_GET;
        case TypeTags.DOUBLE : return DARRAY_GET;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    /** Return set method for arrays of the given type. */
    public Symbol getArraySetSymbol(Type type) {
        switch (type) {
        case UnboxedArrayType(UnboxedType(int kind)):
            return getArraySetSymbol(kind);
        case UnboxedArrayType(_):
            return OARRAY_SET;
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    /** Return set method for arrays of elements of the given kind. */
    public Symbol getArraySetSymbol(int kind) {
        switch (kind) {
        case TypeTags.BOOLEAN: return ZARRAY_SET;
        case TypeTags.BYTE   : return BARRAY_SET;
        case TypeTags.SHORT  : return SARRAY_SET;
        case TypeTags.CHAR   : return CARRAY_SET;
        case TypeTags.INT    : return IARRAY_SET;
        case TypeTags.LONG   : return LARRAY_SET;
        case TypeTags.FLOAT  : return FARRAY_SET;
        case TypeTags.DOUBLE : return DARRAY_SET;
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    //########################################################################
    // Primitives interface - class names

    /* Return name to use in "Class.forName(<name>)" for the given type. */
    public String getNameForClassForName(Type type) {
        switch (type) {
        case TypeRef(_, Symbol symbol, _):
            return getNameForClassForName(symbol);
        case UnboxedType(int kind):
            return getNameForClassForName(kind);
        case UnboxedArrayType(TypeRef(_, Symbol symbol, _)):
            return "[L" + getNameForClassForName(symbol) + ";";
        case UnboxedArrayType(Type elemtp):
            return "[" + getNameForClassForName(elemtp);
        default:
            throw Debug.abort("illegal case", type);
        }
    }

    /* Return name to use in "Class.forName(<name>)" for the given symbol. */
    public String getNameForClassForName(Symbol symbol) {
        return getJREClassName(symbol);
    }

    /* Return name to use in "Class.forName(<name>)" for the given kind. */
    public String getNameForClassForName(int kind) {
        switch (kind) {
        case TypeTags.BOOLEAN: return "Z";
        case TypeTags.BYTE   : return "B";
        case TypeTags.SHORT  : return "S";
        case TypeTags.CHAR   : return "C";
        case TypeTags.INT    : return "I";
        case TypeTags.LONG   : return "J";
        case TypeTags.FLOAT  : return "F";
        case TypeTags.DOUBLE : return "D";
        default              : throw Debug.abort("illegal kind " + kind);
        }
    }

    /** Return the JRE name of given class. */
    public String getJREClassName(Symbol clasz) {
        assert clasz.isClassType(): Debug.show(clasz);
        if (clasz == definitions.ANY_CLASS ||
            clasz == definitions.ANYREF_CLASS)
            return getJREClassName(definitions.OBJECT_CLASS);
        String suffix = clasz.isModuleClass() && !clasz.isJava() ? "$" : "";
        return jreNameWriter.toString(clasz, suffix);
    }

    /** Return the CLR name of given class. */
    public String getCLRClassName(Symbol clasz) {
        assert clasz.isClassType(): Debug.show(clasz);
        return clrNameWriter.toString(clasz);
    }

    //########################################################################
}
