package scala.collection.immutable;

import scala.collection.mutable.Builder;

/** A subclass of `::` which routes access to the usually mutable `tl` var through a final val.
 * This ensures thread-safe publishing of the tail (cf. SI-7838).
 */
final class SafeCons extends $colon$colon {
  private final List tl2;

  public SafeCons(Object _head, List<?> _tail) {
    super(_head, _tail);
    this.tl2 = _tail;
    // freeze action for `tl2` happens here
  }

  // Scala generates two identical accessor methods which read the `tl` field directly.
  // We have to override both of them. All other generated methods go through one of
  // these accessors:
  @Override public List tl$access$1() { return tl2; }
  @Override public List tl() { return tl2; }

  // Disambiguate `newBuilder`, which is inherited twice
  @Override public Builder newBuilder() { return super.newBuilder(); }
}
