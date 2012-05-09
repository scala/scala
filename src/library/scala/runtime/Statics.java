package scala.runtime;

/** Not for public consumption.  Usage by the runtime only.
 */

public final class Statics {
  public static int mix(int hash, int data) {
    int h = mixLast(hash, data);
    h = Integer.rotateLeft(h, 13);
    return h * 5 + 0xe6546b64;
  }

  public static int mixLast(int hash, int data) {
    int k = data;

    k *= 0xcc9e2d51;
    k = Integer.rotateLeft(k, 15);
    k *= 0x1b873593;

    return hash ^ k;
  }

  public static int finalizeHash(int hash, int length) {
    return avalanche(hash ^ length);
  }

  /** Force all bits of the hash to avalanche. Used for finalizing the hash. */
  public static int avalanche(int h) {
    h ^= h >>> 16;
    h *= 0x85ebca6b;
    h ^= h >>> 13;
    h *= 0xc2b2ae35;
    h ^= h >>> 16;

    return h;
  }

  public static int longHash(long lv) {
    if ((int)lv == lv)
      return (int)lv;
    else
      return (int)(lv ^ (lv >>> 32));
  }

  public static int doubleHash(double dv) {
    int iv = (int)dv;
    if (iv == dv)
      return iv;

    float fv = (float)dv;
    if (fv == dv)
      return java.lang.Float.floatToIntBits(fv);

    long lv = (long)dv;
    if (lv == dv)
      return (int)lv;

    lv = Double.doubleToLongBits(dv);
    return (int)(lv ^ (lv >>> 32));
  }

  public static int floatHash(float fv) {
    int iv = (int)fv;
    if (iv == fv)
      return iv;

    long lv = (long)fv;
    if (lv == fv)
      return (int)(lv^(lv>>>32));

    return java.lang.Float.floatToIntBits(fv);
  }

  public static int anyHash(Object x) {
    if (x == null)
      return 0;

    if (x instanceof java.lang.Long)
      return longHash(((java.lang.Long)x).longValue());

    if (x instanceof java.lang.Double)
      return doubleHash(((java.lang.Double)x).doubleValue());

    if (x instanceof java.lang.Float)
      return floatHash(((java.lang.Float)x).floatValue());

    return x.hashCode();
  }
}
