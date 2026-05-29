public class Test {
	static class C<@NotNull T> {};
	@NotNull String foo() { return ""; }
	String @NotNull [] values;
	String @NotNull [] @NotNull [] nestedValues;
	void varargs(String @NotNull ... values) {}
}
