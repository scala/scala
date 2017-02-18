// https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.28
public class Constants {
    public static final int ConstInt = 1;

    public static final int ConstIdent = ConstInt;
    public static final int ConstSelect = Constants.ConstInt;

    // this is a known limitation in scala's javac parser for constants, it will be treated as -1.
    // the java compiler will flag an error.
    public static final int NegatedInt = !1;

    public static final int     ConstOpExpr1 = 1 + 2;
    public static final int     ConstOpExpr2 = 1 << 2;
    public static final boolean ConstOpExpr3 = 1 == 1;
    public static final int     ConstOpExpr4 = true ? 1 : 2;

    public static int NonFinalConst = 1;
    public final int NonStaticConst = 1;
    public int NonConst = 1;

    public static final short   ConstCastExpr = (short)(1*2*3*4*5*6);

    public static final String ConstString = "a";
    public static final String StringAdd = "a" + 1;
}
