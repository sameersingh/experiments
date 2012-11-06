package org.sameersingh.experiments

import collection.mutable.HashMap
import io.Source
import java.util.zip.GZIPInputStream
import java.io.FileInputStream

/**
 * @author sameer
 * @date 10/23/12
 */
class AggregatePoint(val fixed: Point) {
  val spec = fixed.spec
  val mean = new Point(spec)
  val M2 = new Point(spec)
  var n: Double = 0.0

  def zero: AggregatePoint = new AggregatePoint(fixed)

  def double(colId: Int): Double = {
    if (fixed.map.contains(colId)) fixed.double(colId)
    else mean.double(colId)
  }

  def double(shortName: String): Double = double(spec.getId(shortName))

  def accumulate(p: Point): Unit = {
    n += 1
    for (colId <- p.map.keys) {
      if (fixed.map.contains(colId)) {
        // TODO this should never happen!
        throw new Error("fixed column in a point that should be aggregated")
      } else {
        val x = p.double(colId)
        val mv = mean.double(colId)
        val m2 = M2.double(colId)
        // delta = x - mean
        // mean = mean + delta/n
        // M2 = M2 + delta*(x - mean)
        val delta = x - mv
        mean(colId) = mv + delta / n
        M2(colId) = m2 + delta * (x - mean.double(colId))
      }
    }
  }

  def variance: Point = {
    val p = new Point(spec)
    for (c <- M2.map.keys) {
      // variance = M2/(n - 1)
      p(c) = M2.double(c) / (n - 1.0)
    }
    p
  }

  def count = n

  def toLines: Seq[String] =
    Seq("fixed\t" + fixed.toLine(),
      "mean\t" + mean.toLine(),
      "variance\t" + variance.toLine(),
      "M2\t" + M2.toLine(),
      "n\t" + n.toString)
}

object AggregateExperiments {
  def aggregate(exps: Seq[Experiment], fixedCols: Seq[String], aggrOver: Seq[String], cond: (Point) => Boolean = p => true): Seq[AggregatePoint] = {
    val aggre: HashMap[Point, AggregatePoint] = new HashMap
    val fixedColIds = fixedCols.map(exps.head.spec.getId(_))
    val aggrOverIds = aggrOver.map(exps.head.spec.getId(_))
    for (exp <- exps) {
      for (p <- exp.points) {
        if (cond(p)) {
          val fixed = p.copyTrunc(fixedColIds)
          val toAggr = p.copyTrunc(aggrOverIds)
          aggre.getOrElseUpdate(fixed, new AggregatePoint(fixed)).accumulate(toAggr)
        }
      }
    }
    aggre.values.toSeq
  }

  def fromLines(spec: Spec, lines: Seq[String]): AggregatePoint = {
    assert(lines.length == 5)
    // fixed
    val fixedSplit = lines(0).split("\t")
    assert(fixedSplit.length == 2)
    assert(fixedSplit(0) == "fixed")
    val fixed = Point.fromLine(fixedSplit(1), spec)
    val agg = new AggregatePoint(fixed)
    val meanSplit = lines(1).split("\t")
    assert(meanSplit.length == 2)
    assert(meanSplit(0) == "mean")
    agg.mean.fromLine(meanSplit(1))
    val m2Split = lines(1).split("\t")
    assert(m2Split.length == 2)
    assert(m2Split(0) == "M2")
    agg.M2.fromLine(m2Split(1))
    val nSplit = lines(1).split("\t")
    assert(nSplit.length == 2)
    assert(nSplit(0) == "n")
    agg.n = nSplit(1).toDouble
    agg
  }

  def fromFile(spec: Spec, filename: String, gzip: Boolean = false): Seq[AggregatePoint] = {
    val source = if (gzip) Source.fromInputStream(new GZIPInputStream(new FileInputStream(filename))) else Source.fromFile(filename)
    source.getLines().grouped(5).map(ls => fromLines(spec, ls)).toSeq
  }
}