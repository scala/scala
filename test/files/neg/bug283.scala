// bug contribution #12

trait P[a<:P[a,t1],t1]:a;

object X
{
  type A[t1] = P[A[t1],t1];
}
