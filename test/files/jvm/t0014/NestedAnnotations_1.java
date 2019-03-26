package test;

import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

public class NestedAnnotations_1 {
  @OuterAnno(inner=@InnerAnno(name="inner"))
  String field;

  @Target(value={ElementType.FIELD})
  @Retention(value=RetentionPolicy.RUNTIME)
  public static @interface OuterAnno {
    public InnerAnno inner();
  }

  @Target(value={ElementType.FIELD})
  @Retention(value=RetentionPolicy.RUNTIME)
  public static @interface InnerAnno {
    public String name();
  }
}