package execution

trait ExecutionStrategy {
  def apply(f: => Unit)
}


object Sequential extends ExecutionStrategy {
  implicit val sequential = this
  override def apply(f: => Unit) = f
}

object Pooled extends ExecutionStrategy {
  implicit val pooled = this
  import java.util.concurrent._
  def cpus = Runtime.getRuntime.availableProcessors
  private val pool = new ThreadPoolExecutor(0, cpus, 3, TimeUnit.SECONDS, new LinkedBlockingQueue)
  override def apply(f: => Unit) = pool.submit(new Runnable { override def run = f })
}

object Thread extends ExecutionStrategy {
	override def apply(f: => Unit) = new Thread(new Runnable { override def run = f }).start
}