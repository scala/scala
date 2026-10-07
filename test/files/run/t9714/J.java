
package p;

public class J {
	public static J j = new J();
	public static p.J f() {
		return p.J.j;
	}
	public static p.Module$ module() {
		return p.Module$.MODULE$;
	}
	public static p.Module.Target$ target() {
		return p.Module.Target$.MODULE$;
	}

	public String toString() { return "J"; }
}
