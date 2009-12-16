public class Methods {
  public static <T> GenericInterface<T> getGenericInterface() { return null; }
  public static <T> void acceptGenericInterface(GenericInterface<? super T> gi) { }
}