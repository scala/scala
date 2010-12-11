import java.lang.reflect.*;

public class J_2 {
  public void f1(Class<?> clazz) {
    Field[] fields = clazz.getDeclaredFields();
    for (int i = 0 ; i < fields.length; i++) {
      String name = fields[i].getName();
      if (name.length() >= 7 && name.substring(0, 7).equals("bitmap$")) { }
      else System.out.println("(" + name + "," + fields[i].getGenericType() + ")");
    }
  }
  public void f2(Class<?> clazz) {
    Method[] methods = clazz.getDeclaredMethods();
    for (int i = 0 ; i < methods.length; i++) {
      String name = methods[i].getName();
      if (name.length() >= 7 && name.substring(0, 7).equals("bitmap$")) { }
      else System.out.println("(" + name + "," + methods[i].getGenericReturnType() + ")");
    }
  }

  public void javaRun() {
    f1(One.class);
    f2(One.class);
    f1(Two.class);
    f2(Two.class);
  }
}