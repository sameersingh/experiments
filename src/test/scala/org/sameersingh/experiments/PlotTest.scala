package org.sameersingh.experiments

import org.junit._
import Assert._
import rules.TemporaryFolder
import util.Random
import org.sameersingh.scalaplot.gnuplot.GnuplotPlotter
import collection.mutable.ArrayBuffer
import org.sameersingh.scalaplot.LegendPosX

/**
 * @author sameer
 * @date 10/26/12
 */
@Test
class PlotTest {

  @Test
  def testSingleMem() {
    val spec = SpecTest.spec
    val exp = new Experiment(spec)
    val random = new Random()
    for (i <- 0 until 100) {
      val point = new Point(spec)
      point +=("id", i)
      point +=("run", i / 3)
      point +=("expType", random.nextBoolean().toString)
      point +=("score", i * math.abs(random.nextDouble()))
      exp += point
    }
    val chart = Plotting.plotSingleExpMem(exp, "id", "score", "See Score Improve")
    val plotter = new GnuplotPlotter(chart)
    val testFile = java.io.File.createTempFile("plot", "exps")
    println(testFile.getCanonicalPath)
    plotter.writeToPdf(testFile)
  }

  @Test
  def testMultiMem() {
    val spec = SpecTest.spec
    val random = new Random()
    val numExps = 3
    val exps = new ArrayBuffer[Experiment]()
    for (e <- 0 until numExps) {
      val exp = new Experiment(spec)
      for (i <- 0 until 100) {
        val point = new Point(spec)
        point +=("id", i)
        point +=("run", i / 3)
        point +=("expType", "exp" + e)
        point +=("score", i * math.abs(random.nextDouble()))
        exp += point
      }
      exps += exp
    }
    val chart = Plotting.plotExpMem(exps, "id", "score", "expType", "See Score Improve")
    chart.legendPosX = LegendPosX.Left
    val plotter = new GnuplotPlotter(chart)
    val testFile = java.io.File.createTempFile("plot", "exps")
    println(testFile.getCanonicalPath)
    plotter.writeToPdf(testFile)
  }

}
