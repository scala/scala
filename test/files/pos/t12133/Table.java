package pkg;

public @interface Table {
  String name();
  UniqueConstraint[] uniqueConstraints();
}
