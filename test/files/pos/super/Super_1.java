// A.java
interface Inter<T> { }

class Super implements Inter<Super.Inner> {  
  public class Inner { };
}
