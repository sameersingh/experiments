package org.sameersingh.experiments

import collection.mutable.HashMap

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
}

object AggregateExperiments {
  def aggregate(exps: Seq[Experiment], fixedCols: Seq[String], aggrOver: Seq[String]): Seq[AggregatePoint] = {
    val aggre: HashMap[Point, AggregatePoint] = new HashMap
    val fixedColIds = fixedCols.map(exps.head.spec.getId(_))
    val aggrOverIds = aggrOver.map(exps.head.spec.getId(_))
    for (exp <- exps) {
      for (p <- exp.points) {
        val fixed = p.copyTrunc(fixedColIds)
        val toAggr = p.copyTrunc(aggrOverIds)
        aggre.getOrElseUpdate(fixed, new AggregatePoint(fixed)).accumulate(toAggr)
      }
    }
    aggre.values.toSeq
  }
}