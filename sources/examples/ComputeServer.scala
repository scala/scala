package examples;

import concurrent._, concurrent.ops._;

class ComputeServer(n: Int) {
  private trait Job {
    type t;
    def task: t;
    def return(x: t): Unit;
  }

  private val openJobs = new Channel[Job]();

  private def processor(i: Int): Unit {
    while (True) {
      val job = openJobs.read;
      job.return(job.task)
    }
  }
  def future[a](def p: a): () => a {
    val reply = new SyncVar[a]();
    openJobs.write{
      new Job with {
	type t = a;
	def task = p;
	def return(x: a) = reply.set(x);
      }
    }
    () => reply.get
  }
  replicate(1,n){processor}
}
