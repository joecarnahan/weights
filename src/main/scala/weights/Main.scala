/**
 * Main program for computing what weights can be assembled from a given set
 * of plates.
 *
 * @author Joe Carnahan
 */

package weights

import java.io.BufferedWriter
import java.io.FileWriter

object Main {
  val minSteps = List(1.0, 2.0, 4.0)

  def main(args: Array[String]) = 
    minSteps.map((minStep: Double) => {
      val filename = "steps_" + minStep.toString + ".txt"
      val writer = new BufferedWriter(new FileWriter(filename))
      try {
        writer.write((new Weights(minStep, args)).calculatePossibilities)
        writer.newLine
      }
      finally {
        writer.close
      }
    })
}
