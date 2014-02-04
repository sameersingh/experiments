package org.sameersingh.experiments

import collection.mutable.HashMap
import org.sameersingh.utils.misc.Json

/**
 * A single set of experiment results, a "data point" for the results
 * @author sameer
 * @date 10/6/12
 */
case class Point(val spec: Spec) {
  // map of column id to the data
  val map = new HashMap[Int, Any]

  def apply(colId: Int): Any = map.getOrElse(colId, spec(colId).defaultValue)

  def apply(shortName: String): Any = apply(spec.getId(shortName))

  def double(colId: Int) = spec(colId).valueToDouble(this(colId))

  def double(shortName: String): Double = double(spec.getId(shortName))

  def value[T](colId: Int): T = apply(colId).asInstanceOf[T]

  def value[T](shortName: String): T = value[T](spec.getId(shortName))

  def update(colId: Int, value: Any): Any = map(colId) = value

  def update(shortName: String, value: Any): Any = this +=(shortName, value)

  def +=(colId: Int, value: Any): Unit = {
    assert(!map.contains(colId))
    map(colId) = value
  }

  def +=(shortName: String, value: Any): Unit = {
    +=(spec.getId(shortName), value)
  }

  def toLine(verbose: Boolean = false): String = {
    def printCol(cid: Int): String = if (verbose) spec(cid).shortName else cid.toString
    def print(cid: Int, v: Any): String = if (verbose) v.toString else spec(cid).valueToString(v)
    /*val sb = new StringBuffer()
    var first = true
    for (value: Pair[Int, Any] <- map.toList.sortBy(_._1)) {
      if (first) {
        sb.append("%d:%s".format(value._1, print(value._1, value._2)))
        first = false
      } else sb.append("\t%d:%s".format(value._1, print(value._1, value._2)))
    }
    sb.toString*/
    Json.generate(map.map(value => (printCol(value._1), print(value._1, value._2))))
  }

  def copyTrunc(colIds: Iterable[Int]): Point = {
    val point = new Point(spec)
    for (colId <- colIds) {
      point +=(colId, this(colId))
    }
    point
  }

  def fromLine(line: String, excludes: Set[Int] = Set.empty): Unit = {
    map.clear
    /*for (pointStr: String <- line.split("\t").toSeq) {
      val split = pointStr.split(":")
      assert(split.length == 2)
      val colId = split(0).toInt
      this +=(colId, spec(colId).valueFromString(split(1)))
    }*/
    for ((col, value) <- Json.parse[Map[String, String]](line)) {
      val colId = col.toInt
      if (excludes.size == 0 || !excludes(colId))
        map(colId) = spec(colId).valueFromString(value)
    }
  }

  override def hashCode() = map.toSeq.hashCode()

  override def equals(p1: Any) = p1 match {
    case p: Point => (p.map.size == map.size) &&
          map.forall(v => p.map(v._1).toString equals v._2.toString) &&
          p.map.forall(v => map(v._1).toString equals v._2.toString) //(p.spec equals spec) &&
    case _ => false
  }
}

object Point {
  def fromLine(line: String, spec: Spec, excludes: Set[Int] = Set.empty): Point = {
    val point = new Point(spec)
    point.fromLine(line, excludes)
    point
  }
}