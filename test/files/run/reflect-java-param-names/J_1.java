/*
 * javac: -parameters
 */
public class J_1<T> {
    public J_1(int i, int j) {}
    public <J extends T> void inst(int i, J j) {}
    public static <J> void statik(int i, J j) {}
}