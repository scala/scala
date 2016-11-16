/*
 * javac: -encoding UTF-8
 */
public class C_1 {
    public static final String NULLED = "X\000ABC";
    public static final String SUPPED = "𐒈𐒝𐒑𐒛𐒐𐒘𐒕𐒖";

    public String nulled() {
        return C_1.NULLED;
    }
    public String supped() {
        return C_1.SUPPED;
    }
    public int nulledSize() {
        return C_1.NULLED.length();
    }
    public int suppedSize() {
        return C_1.SUPPED.length();
    }
}
