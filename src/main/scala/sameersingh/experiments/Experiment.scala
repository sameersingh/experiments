package sameersingh.experiments

import collection.mutable.{ArrayBuffer, Seq}
import io.Source
import java.io.{FileOutputStream, FileInputStream, PrintWriter}
import java.util.zip.{GZIPOutputStream, GZIPInputStream}
import collection.mutable

/**
 * Set of points that form an experiment
 * @author sameer
 * @date 10/6/12
 */
class Experiment(val spec: Spec) extends mutable.Buffer[Point] {
  val points: ArrayBuffer[Point] = new ArrayBuffer


  override def filter(p: (Point) => Boolean): Experiment = {
    val exp = new Experiment(spec)
    for (point <- points) {
      if (p(point)) exp += point
    }
    exp
  }

  def copyAndTrunc(shortNames: Set[String]): Experiment = {
    val colIds = shortNames.map(s => spec.getId(s)).toSeq
    val exp = new Experiment(spec)
    for (point <- points) {
      exp += point.copyTrunc(colIds)
    }
    exp
  }

  def fromFile(filename: String, gzip: Boolean = false) {
    val source = if (gzip) Source.fromInputStream(new GZIPInputStream(new FileInputStream(filename))) else Source.fromFile(filename)
    for (line <- source.getLines())
      this += Point.fromLine(line, spec)
  }

  def toFile(filename: String, gzip: Boolean = false) {
    val outputStream = if (gzip) new GZIPOutputStream(new FileOutputStream(filename)) else new FileOutputStream(filename)
    val writer = new PrintWriter(outputStream)
    for (point <- points) {
      writer.println(point.toLine)
    }
    writer.flush
    writer.close
  }

  // Seq methods
  def update(idx: Int, elem: Point) { points.update(idx, elem) }

  def length = points.length

  def apply(idx: Int) = points(idx)

  def iterator = points.iterator

  def +=(elem: Point) = {
    points += elem
    this
  }

  def clear() = points.clear()

  def +=:(elem: Point) = {
    points.+=:(elem)
    this
  }

  def insertAll(n: Int, elems: Traversable[Point]) = points.insertAll(n, elems)

  def remove(n: Int) = points.remove(n)
}