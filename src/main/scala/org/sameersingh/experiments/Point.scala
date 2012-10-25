package org.sameersingh.experiments

import collection.mutable.HashMap

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

  def toLine: String = {
    val sb = new StringBuffer()
    var first = true
    for (value: Pair[Int, Any] <- map.toList.sortBy(_._1)) {
      if (first) {
        sb.append("%d:%s".format(value._1, spec(value._1).valueToString(value._2)))
        first = false
      } else sb.append("\t%d:%s".format(value._1, spec(value._1).valueToString(value._2)))
    }
    sb.toString
  }

  def copyTrunc(colIds: Iterable[Int]): Point = {
    val point = new Point(spec)
    for (colId <- colIds) {
      point +=(colId, this(colId))
    }
    point
  }

  def fromLine(line: String): Unit = {
    map.clear
    for (pointStr: String <- line.split("\t").toSeq) {
      val split = pointStr.split(":")
      assert(split.length == 2)
      val colId = split(0).toInt
      this +=(colId, spec(colId).valueFromString(split(1)))
    }
  }


  override def hashCode() = map.hashCode()

  override def equals(p1: Any) = p1 match {
    case p: Point => (p.spec equals spec) && (p.map equals map)
    case _ => false
  }
}

object Point {
  def fromLine(line: String, spec: Spec): Point = {
    val point = new Point(spec)
    point.fromLine(line)
    point
  }
}