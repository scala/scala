package test;
@Test
public class Foo {
  public static void main(String[] args){
    System.out.println(Foo.class.getAnnotations().length);
  }
}
