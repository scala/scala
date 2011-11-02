package test;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

public class NestedAnnotations {

  @OuterAnno(inner=@InnerAnno(name="inner"))
  String field;
  
  @Target({FIELD}) 
  @Retention(RUNTIME)
  public static @interface InnerAnno {
    String name();
  }
  
  @Target({FIELD}) 
  @Retention(RUNTIME)
  public static @interface OuterAnno {
    InnerAnno inner();
  }
}
