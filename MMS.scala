import scala.util.Random
import time.Time; import Time._

class MMS(lam:Double, mu:Double, s:Int) extends Simulation {
  import Distributions._

  type ListBuffer = scala.collection.mutable.ListBuffer[Int]

  var que = new ListBuffer()

  var num = 40;
  var busy = new Array[Boolean](s)

  val arrivals: IntDistribution = new PoissonDistribution(lam).scale(SEC).sum().toInt
  val service: IntDistribution = new PoissonDistribution(mu).scale(SEC).sum().toInt

  def initialize():Unit = {
    for {i <- 1 to num} after(Time(arrivals.next)) {arrive(i)}
  }

  override def log(message: Any): Unit = println(s"@$now: $message")

  def arrive(who:Int):Unit = {
    log(s"#$who arrived")
    var havetowait = true
    for (i <- 0 until s) {
      if (!busy(i) && havetowait) {
        serve(who, i)
        havetowait = false
      }
    }
    if (havetowait) que.append(who)
  }

  def serve(who:Int, server:Int):Unit = {
    busy(server) = true
    log(s"#$who being served at counter #$server")
    after(Time(service.next)) {
      leave(who, server)
    }
  }

  def leave(who:Int, server:Int):Unit = {
    log(s"#$who leaving counter #$server")
    busy(server) = false
    if (!que.isEmpty) {
      val nextperson = que(0)
      que = que.drop(1)
      serve(nextperson, server)
    }
  }


}

object MMS_1 {
  def main(args:Array[String]):Unit = {
    var q = new MMS(20.0, 20.0, 4)
    q.run(Some(secs(50)))
  }
}
