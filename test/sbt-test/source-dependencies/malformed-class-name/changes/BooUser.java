package repro;

import repro.Boo;
import repro.Boo2;
import repro.Boo3;
import repro.Boo4;

public class BooUser {
  private static Boo getOwnBuf() {
    return new Boo() {};
  }

  private static Boo2 getOwnBuf2() {
    return new Boo2() {};
  }

  private static Boo3 getOwnBuf3() {
    return new Boo3() {};
  }

  private static Boo4 getOwnBuf4() {
    return new Boo4() {};
  }

  public static void main(String[] args) {
    Boo boo = getOwnBuf();
    Boo2 boo2 = getOwnBuf2();
    Boo3 boo3 = getOwnBuf3();
    Boo4 boo4 = getOwnBuf4();
    System.out.println("Made a buf: " + boo);
  }
}
