// javaVersion: 17+
package p;

sealed abstract public class X {
}

final class W extends X {
}

final class Y extends X {
}

sealed class Z extends X permits Z1, Z2 {
}

final class Z1 extends Z {
}

final class Z2 extends Z {
}
