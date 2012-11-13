package org.sameersingh.experiments

import org.junit._
import Assert._
import collection.mutable.ArrayBuffer
import util.Random

/**
 * @author sameer
 * @date 10/25/12
 */
@Test
class AggregateTest {

  @Test
  def testNoneFixedSingleDoubleColumn(): Unit = {
    val numPoints = 1000
    val trueMean = 100.0
    val trueVariance = 10.0

    val spec = new Spec
    spec.addDoubleColumn("d", "Double")

    val random = new Random()
    val std = math.sqrt(trueVariance)
    val exp = new Experiment(spec)
    var mean = 0.0
    var M2 = 0.0
    var n = 0.0
    for (i <- 0 until numPoints) {
      val x = random.nextGaussian() * std + trueMean
      n += 1.0
      val delta = x - mean
      mean = mean + delta / n
      M2 = M2 + delta * (x - mean)
      val p = new Point(spec)
      p +=("d", x)
      exp += (p)
    }
    var variance = M2 / (n - 1.0)
    assertEquals(trueMean, mean, 2.0)
    assertEquals(trueVariance, variance, 2.0)

    val aggs = AggregateExperiments.aggregate(Seq(exp), Seq.empty, Seq("d"))
    assertEquals(1, aggs.size)
    assertEquals(mean, aggs.head.mean.double("d"), 0.0000001)
    assertEquals(variance, aggs.head.variance.double("d"), 0.0000001)
  }

  @Test
  def testSingleFixedSingleDoubleColumn(): Unit = {
    val numPoints = 5000
    val trueMean1 = 100.0
    val trueVariance1 = 10.0
    val trueMean2 = -100.0
    val trueVariance2 = 20.0
    val std1 = math.sqrt(trueVariance1)
    val std2 = math.sqrt(trueVariance2)

    val spec = new Spec
    spec.addDoubleColumn("d", "Double")
    spec.addStringColumn("s", "String")
    spec.addStringColumn("f", "String")

    val random = new Random()
    val exp = new Experiment(spec)
    var mean1 = 0.0
    var M21 = 0.0
    var n1 = 0.0
    var mean2 = 0.0
    var M22 = 0.0
    var n2 = 0.0
    var mean = 0.0
    var M2 = 0.0
    var n = 0.0
    for (i <- 0 until numPoints) {
      val p = new Point(spec)
      var x = 0.0
      if (random.nextBoolean()) {
        p +=("s", "true")
        x = random.nextGaussian() * std1 + trueMean1
        n1 += 1.0
        val delta = x - mean1
        mean1 = mean1 + delta / n1
        M21 = M21 + delta * (x - mean1)
      } else {
        p +=("s", "false")
        x = random.nextGaussian() * std2 + trueMean2
        n2 += 1.0
        val delta = x - mean2
        mean2 = mean2 + delta / n2
        M22 = M22 + delta * (x - mean2)
      }
      n += 1.0
      val delta = x - mean
      mean = mean + delta / n
      M2 = M2 + delta * (x - mean)
      p +=("f", "same")
      p +=("d", x)
      exp += (p)
    }
    var variance1 = M21 / (n1 - 1.0)
    var variance2 = M22 / (n2 - 1.0)
    var variance = M2 / (n - 1.0)
    assertEquals(trueMean1, mean1, 2.0)
    assertEquals(trueVariance1, variance1, 2.0)
    assertEquals(trueMean2, mean2, 2.0)
    assertEquals(trueVariance2, variance2, 2.0)

    val aggs = AggregateExperiments.aggregate(Seq(exp), Seq("s"), Seq("d"))
    assertEquals(2, aggs.size)
    for (agg <- aggs) {
      if (agg.fixed("s") == "true") {
        assertEquals(mean1, agg.mean.double("d"), 0.0000001)
        assertEquals(variance1, agg.variance.double("d"), 0.0000001)
      } else {
        assertEquals("false", agg.fixed("s"))
        assertEquals(mean2, agg.mean.double("d"), 0.0000001)
        assertEquals(variance2, agg.variance.double("d"), 0.0000001)
      }
    }

    val sameAgg = AggregateExperiments.aggregate(Seq(exp), Seq("f"), Seq("d"))
    assertEquals(1, sameAgg.size)
    assertEquals(mean, sameAgg(0).mean.double("d"), 0.0000001)
    assertEquals(variance, sameAgg(0).variance.double("d"), 0.0000001)
  }

  @Test
  def testPointEquals() {
    val spec = new Spec
    spec.addDoubleColumn("d", "Double")
    spec.addStringColumn("s", "String")
    spec.addStringColumn("f", "String")

    val p1 = new Point(spec)
    val p2 = new Point(spec)

    p1("s") = "sameer"
    p2("s") = "sameer"

    assertTrue(p1 == p2)
    assertTrue(p1.hashCode() == p2.hashCode)

    p1("f") = "roxxx"
    p2("f") = "roxxx"

    assertTrue(p1 == p2)
    assertTrue(p1.hashCode() == p2.hashCode)

    p1("d") = 37.0
    p2("d") = 37.0

    assertTrue(p1 == p2)
    assertTrue(p1.hashCode() == p2.hashCode)

  }

}
