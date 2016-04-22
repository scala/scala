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
    int iv = (int)lv;
    if (iv == lv)
      return iv;

    return java.lang.Long.hashCode(lv);
  }

  public static int doubleHash(double dv) {
    int iv = (int)dv;
    if (iv == dv)
      return iv;

    long lv = (long)dv;
    if (lv == dv)
      return java.lang.Long.hashCode(lv);

    float fv = (float)dv;
    if (fv == dv)
      return java.lang.Float.hashCode(fv);

    return java.lang.Double.hashCode(dv);
  }

  public static int floatHash(float fv) {
    int iv = (int)fv;
    if (iv == fv)
      return iv;

    long lv = (long)fv;
    if (lv == fv)
      return java.lang.Long.hashCode(lv);

    return java.lang.Float.hashCode(fv);
  }

  /**
   * Hashcode algorithm is driven by the requirements imposed
   * by primitive equality semantics, namely that equal objects
   * have equal hashCodes.  The first priority are the integral/char
   * types, which already have the same hashCodes for the same
   * values except for Long.  So Long's hashCode is altered to
   * conform to Int's for all values in Int's range.
   *
   * Float is problematic because it's far too small to hold
   * all the Ints, so for instance Int.MaxValue.toFloat claims
   * to be == to each of the largest 64 Ints.  There is no way
   * to preserve equals/hashCode alignment without compromising
   * the hashCode distribution, so Floats are only guaranteed
   * to have the same hashCode for whole Floats in the range
   * Short.MinValue to Short.MaxValue (2^16 total.)
   *
   * Double has its hashCode altered to match the entire Int range,
   * but is not guaranteed beyond that.  (But could/should it be?
   * The hashCode is only 32 bits so this is a more tractable
   * issue than Float's, but it might be better simply to exclude it.)
   *
   * Note: BigInt and BigDecimal, being arbitrary precision, could
   * be made consistent with all other types for the Int range, but
   * as yet have not.
   *
   * Note: Among primitives, Float.NaN != Float.NaN, but the boxed
   * versions are equal.  This still needs reconciliation.
   */
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
