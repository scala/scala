package a;

public abstract class A {
  abstract String foo(); // package protected!
  public void run() {
   System.out.println(foo());
  }
}