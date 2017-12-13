public @interface Ann {
  public String bar();
  public String[] quux() default {};
  public SuppressWarnings baz() default @SuppressWarnings({});
}
