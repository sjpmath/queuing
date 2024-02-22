
object Distributions {
  import java.util.Random

  trait Distribution[N]  { host: Distribution[N] =>
     def next: N
     def map[T](f: N => T): Distribution[T] = new Distribution[T] {
         def next: T = f(host.next)
     }
  }  
  
  type  IntDistribution = Distribution[Int]
  
  trait RealDistribution extends Distribution[Double]
  { host: RealDistribution =>  // bound to this RealDistribution itself; referenceable from inner classes

    def toInt: IntDistribution = map { d => Math.round(d).toInt }
    
    def scale(factor: Long): RealDistribution   = scale(factor.toDouble)
   
    def scale(factor: Double): RealDistribution = new RealDistribution { def next: Double = factor*host.next }

    def add(offset: Double): RealDistribution   = new RealDistribution { def next: Double = offset+host.next }

    def sum(offset: Double = 0.0): RealDistribution = new RealDistribution {
      private var sum = offset
      def next: Double = {sum += host.next; sum}
    }
  }
  /** A Flat distribution generates equiprobable `Double`s between 0.0 and 1.0 */
  class FlatDistribution(seed: Long=666) extends RealDistribution {
    private val random = new Random(seed)
    def next: Double = random.nextDouble
  }
  
  /** 
   *  A Gaussian normal distribution with given mean and standard deviation: generated
   *  by translating and scaling the standard library Gaussian distribution (mean=0.0, stddev=1.0) 
   */
  class NormalDistribution(mean: Double, stddev: Double, seed: Long=666) extends RealDistribution {
    private val random = new Random(seed)
    def next: Double = mean + stddev*random.nextGaussian
  }

  /** A Poisson distribution.
   *  Generates times `between` successive events.
   *  Specified by the mean event rate per unit time.
   *  The times of `successive` events are given by `aPoisson.sum(0)`
   */
  class PoissonDistribution(rate: Double, seed: Long=666) extends RealDistribution {
    private val random = new Random(seed)
    def next: Double = (-1.0/rate) * Math.log(random.nextDouble)
  }
}