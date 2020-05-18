public @interface Ann {
  Class<?>[] classes() default {};
  String[] strings() default {};
  String string() default "";
  int integer() default 0;
  int[] integers() default {};
}
