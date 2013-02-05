package org.sameersingh.experiments

import org.sameersingh.scalaplot._
import collection.mutable.ArrayBuffer
import collection.mutable.HashMap

/**
 * @author sameer
 * @date 10/26/12
 */
object Plotting {

  def getMemXYSeries(exp: Experiment, xcol: String, ycol: String): MemXYSeries = getMemXYSeries(exp, xcol, ycol, exp.spec(ycol).fullName)

  def getMemXYSeries(experiment: Experiment, xcol: String, ycol: String, seriesName: String): MemXYSeries = {
    val xcolId = experiment.spec.getId(xcol)
    val ycolId = experiment.spec.getId(ycol)
    val xs = new ArrayBuffer[Double]()
    val ys = new ArrayBuffer[Double]()
    for (p <- experiment.points) {
      xs += p.double(xcolId)
      ys += p.double(ycolId)
    }
    val s = new MemXYSeries(xs, ys, seriesName)
    s.plotStyle = XYPlotStyle.Lines
    s
  }

  def plotSingleExpMem(experiment: Experiment, xcol: String, ycol: String): XYChart = {
    plotSingleExpMem(experiment, xcol, ycol, "%s vs %s" format(experiment.spec(ycol).fullName, experiment.spec(xcol).fullName))
  }

  def plotSingleExpMem(experiment: Experiment, xcol: String, ycols: Seq[String], ytitle: String, chartTitle: String): XYChart = {
    val serieses = new ArrayBuffer[XYSeries]
    for (ycol <- ycols) {
      serieses += getMemXYSeries(experiment, xcol, ycol)
    }
    val data = new XYData(serieses: _*)
    val chart = new XYChart(chartTitle, data)
    chart.xlabel = experiment.spec(xcol).fullName
    chart.ylabel = ytitle
    chart.size = Some((3.75, 3.0))
    chart.showLegend = true
    chart
  }

  def plotSingleExpMem(experiment: Experiment, xcol: String, ycol: String, chartTitle: String): XYChart = {
    val series = getMemXYSeries(experiment, xcol, ycol)
    val data = new XYData(series)
    val chart = new XYChart(chartTitle, data)
    chart.xlabel = experiment.spec(xcol).fullName
    chart.ylabel = experiment.spec(ycol).fullName
    chart.size = Some((3.75, 3.0))
    chart.showLegend = false
    chart
  }

  def plotExpMem(exps: Seq[Experiment], xcol: String, ycol: String, seriesNameCol: String): XYChart = {
    plotExpMem(exps, xcol, ycol, seriesNameCol, "%s vs %s" format(exps.head.spec(ycol).fullName, exps.head.spec(xcol).fullName))
  }

  def plotExpMem(exps: Seq[Experiment], xcol: String, ycol: String, seriesNameCol: String, chartTitle: String): XYChart = {
    val data = new XYData()
    for (exp <- exps) {
      data += getMemXYSeries(exp, xcol, ycol, exp.points.head(seriesNameCol).toString)
    }
    val chart = new XYChart(chartTitle, data)
    chart.xlabel = exps.head.spec(xcol).fullName
    chart.ylabel = exps.head.spec(ycol).fullName
    chart.size = Some((3.75, 3.0))
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

    val data = new XYData()
    // series name -> x and y
    val smap = new HashMap[String, (ArrayBuffer[Double], ArrayBuffer[Double])]
    for (p <- aggrs) {
      val (xs, ys) = smap.getOrElseUpdate(p.fixed(seriesNameCol).toString, Pair(new ArrayBuffer, new ArrayBuffer))
      xs += p.double(xcolId)
      ys += p.double(ycolId)
    }
    //println
    for ((sname, (xs, ys)) <- smap) {
      val sorted = (xs zip ys).sortBy(p => p._1)
      data += new MemXYSeries(sorted.map(_._1), sorted.map(_._2), sname)
    }
    val chart = new XYChart(chartTitle, data)
    chart.xlabel = spec(xcol).fullName
    chart.ylabel = spec(ycol).fullName
    chart.size = Some((3.75, 3.0))
    chart.showLegend = true
    chart
  }

}
