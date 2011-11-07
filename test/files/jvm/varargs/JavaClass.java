


public class JavaClass {
    public static <T> void varargz(int i, T... v) {
    }
    
    public static void callSomeAnnotations() {
	VaClass va = new VaClass();
	va.vs(4, "", "", "");
	va.vi(1, 2, 3, 4);
        varargz(5, 1.0, 2.0, 3.0);
	va.vt(16, "", "", "");
    }
}