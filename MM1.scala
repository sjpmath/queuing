import scala.util.Random
import time.Time; import Time._

class MM1(lam:Double, mu:Double) extends Simulation {
  import Distributions._

  type ListBuffer = scala.collection.mutable.ListBuffer[Int]

  var que = new ListBuffer()

  var num = 20;
  var busy = false;

  val arrivals: IntDistribution = new PoissonDistribution(lam).scale(SEC).sum().toInt
  val service: IntDistribution = new PoissonDistribution(mu).scale(SEC).sum().toInt

  def initialize():Unit = {
    for {i <- 1 to num} after(Time(arrivals.next)) {arrive(i)}
  }

  override def log(message: Any): Unit = println(s"@$now: $message")

  def arrive(who:Int):Unit = {
    log(s"#$who arrived")
    if (busy) {
      que.append(who)
    } else{
      serve(who)
    }
  }

  def serve(who:Int):Unit = {
    busy = true
    log(s"#$who being served")
    after(Time(service.next)) {
      leave(who)
    }
  }

  def leave(who:Int):Unit = {
    log(s"#$who leaving")
    busy = false
    if (!que.isEmpty) {
      val nextperson = que(0)
      que = que.drop(1)
      serve(nextperson)
    }
  }


}

object MM1_1 {
  def main(args:Array[String]):Unit = {
    var q = new MM1(10.0, 10.0)
    q.run(Some(secs(50)))
  }
}
