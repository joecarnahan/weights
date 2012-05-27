/**
 * Main program for computing what weights can be assembled from a given set
 * of plates.
 *
 * @author Joe Carnahan
 */

package weights

object Main {
  val minStep = 2.0

  def main(args: Array[String]) = 
    println((new Weights(minStep, args)).calculatePossibilities)
}
