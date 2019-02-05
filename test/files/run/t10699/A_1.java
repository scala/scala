/*
 * javac: -parameters
 */
public class A_1 {
    public <T> T identity_inst(T t, T other) { return t; }
    public static <T> T identity_static(T t, T other) { return t; }
}