public class OuterTParams<A> {
    class InnerClass {
	// Cannot parse method signature: "()TA;"
	public A method() { return null; }
    }
}
