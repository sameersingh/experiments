package org.sameersingh.experiments

import org.sameersingh.scalaplot._
import collection.mutable.ArrayBuffer

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

  def plotSingleExpMem(experiment: Experiment, xcol: String, ycol: String, chartTitle: String): XYChart = {
    val series = getMemXYSeries(experiment, xcol, ycol)
    val data = new XYData(experiment.spec(xcol).fullName, experiment.spec(ycol).fullName, Seq(series))
    val chart = new XYChart(chartTitle, data)
    chart.showLegend = false
    chart
  }

  def plotExpMem(exps: Seq[Experiment], xcol: String, ycol: String, seriesCol: String, chartTitle: String): XYChart = {
    val data = new XYData(exps.head.spec(xcol).fullName, exps.head.spec(ycol).fullName)
    for (exp <- exps) {
      data += getMemXYSeries(exp, xcol, ycol, exp.points.head.value[String](seriesCol))
    }
    val chart = new XYChart(chartTitle, data)
    chart.showLegend = true
    chart
  }

}
