package x;

public interface Converter {
    static final String STRING = "STRING";
    abstract class FactoryFactory {
        protected static String getString() { return "string"; }
    }
}