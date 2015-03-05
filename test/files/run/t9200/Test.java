public class Test {
    public static void main(String[] args) {
        new C1(new C2()); // Was NoSuchMethodError
    }
}

