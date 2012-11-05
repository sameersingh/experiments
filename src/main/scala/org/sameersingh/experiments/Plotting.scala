package org.sameersingh.experiments

import org.sameersingh.scalaplot._
import collection.mutable.ArrayBuffer
import collection.mutable.HashMap

/**
 * @author sameer
 * @date 10/26/12
 */
object Plotting {

  def getMemXYSeries(experiment: Experiment, xcol: String, ycol: String, seriesName: String = "default"): MemXYSeries = {
    val xcolId = experiment.spec.getId(xcol)
    val ycolId = experiment.spec.getId(ycol)
    val xs = new ArrayBuffer[Double]()
    val ys = new ArrayBuffer[Double]()
    for (p <- experiment.points) {
      xs += p.double(xcolId)
      ys += p.double(ycolId)
    }
    new MemXYSeries(xs, ys, seriesName)
  }

  def plotSingleExpMem(experiment: Experiment, xcol: String, ycol: String): XYChart = {
    plotSingleExpMem(experiment, xcol, ycol, "% vs %s" format(experiment.spec(ycol).fullName, experiment.spec(xcol).fullName))
  }

  def plotSingleExpMem(experiment: Experiment, xcol: String, ycols: Seq[String], ytitle: String, chartTitle: String): XYChart = {
    val serieses = new ArrayBuffer[XYSeries]
    for (ycol <- ycols) {
      serieses += getMemXYSeries(experiment, xcol, ycol)
    }
    val data = new XYData(experiment.spec(xcol).fullName, ytitle, serieses)
    val chart = new XYChart(chartTitle, data)
    chart.showLegend = true
    chart
  }

  def plotSingleExpMem(experiment: Experiment, xcol: String, ycol: String, chartTitle: String): XYChart = {
    val series = getMemXYSeries(experiment, xcol, ycol)
    val data = new XYData(experiment.spec(xcol).fullName, experiment.spec(ycol).fullName, Seq(series))
    val chart = new XYChart(chartTitle, data)
    chart.showLegend = false
    chart
  }

  def plotExpMem(exps: Seq[Experiment], xcol: String, ycol: String, seriesNameCol: String): XYChart = {
    plotExpMem(exps, xcol, ycol, seriesNameCol, "%s vs %s" format(exps.head.spec(ycol).fullName, exps.head.spec(xcol).fullName))
  }

  def plotExpMem(exps: Seq[Experiment], xcol: String, ycol: String, seriesNameCol: String, chartTitle: String): XYChart = {
    val data = new XYData(exps.head.spec(xcol).fullName, exps.head.spec(ycol).fullName)
    for (exp <- exps) {
      data += getMemXYSeries(exp, xcol, ycol, exp.points.head.value[String](seriesNameCol))
    }
    val chart = new XYChart(chartTitle, data)
    chart.showLegend = true
    chart
  }

  def plotAggregate(aggrs: Seq[AggregatePoint], xcol: String, ycol: String, seriesNameCol: String): XYChart = {
    plotAggregate(aggrs, xcol, ycol, seriesNameCol, "%s vs %s" format(aggrs.head.spec(ycol).fullName, aggrs.head.spec(xcol).fullName))
  }

  def plotAggregate(aggrs: Seq[AggregatePoint], xcol: String, ycol: String, seriesNameCol: String, chartTitle: String): XYChart = {
    val spec: Spec = aggrs.head.spec

    val xcolId = spec.getId(xcol)
    val ycolId = spec.getId(ycol)

    val data = new XYData(spec(xcol).fullName, spec(ycol).fullName)
    // series name -> x and y
    val smap = new HashMap[String, (ArrayBuffer[Double], ArrayBuffer[Double])]
    for (p <- aggrs) {
      val (xs, ys) = smap.getOrElseUpdate(p.fixed.value[String](seriesNameCol), Pair(new ArrayBuffer, new ArrayBuffer))
      xs += p.double(xcolId)
      ys += p.double(ycolId)
    }

    for ((sname, (xs, ys)) <- smap) {
      data += new MemXYSeries(xs, ys, sname)
    }
    val chart = new XYChart(chartTitle, data)
    chart.showLegend = true
    chart
  }

}
