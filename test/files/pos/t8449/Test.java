public class Test {
	// Raw type over a Scala type constructor
	public scala.Function1 foo() { return null; }
	// scalac reported:
	// % scalac-hash v2.11.2 -d /tmp sandbox/{Test.java,Client.scala}
	// sandbox/Test.java:2: error: trait Function1 takes type parameters
	// 	public scala.Function1 foo() { return null; }
	//                      ^
	// one error found
}
