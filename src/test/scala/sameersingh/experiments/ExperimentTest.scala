package sameersingh.experiments

import org.junit._
import Assert._
import rules.TemporaryFolder
import util.Random

/**
 * @author sameer
 * @date 10/6/12
 */
@Test
class ExperimentTest {

  @Test
  def testWrite(): Unit = {
    val testFile = java.io.File.createTempFile("test", "exps")
    println(testFile.getCanonicalPath)
    val spec = SpecTest.spec
    val exp = new Experiment(spec)
    val random = new Random()
    for (i <- 0 until 10) {
      val point = new Point(spec)
      point +=("id", i)
      point +=("run", i / 3)
      point +=("expType", random.nextBoolean().toString)
      point +=("score", random.nextDouble())
      exp += point
    }
    exp.toFile(testFile.getCanonicalPath, true)

    val nexp = new Experiment(spec)
    nexp.fromFile(testFile.getCanonicalPath, true)

    for (point <- nexp) {
      println(point.toLine)
    }
  }

}
