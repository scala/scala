// javaVersion: 17+
package p;

sealed public class H {
}

final class K extends H {
}

non-sealed class L extends H {
}

sealed
class P extends H {
}

final
class Q extends P {
}
