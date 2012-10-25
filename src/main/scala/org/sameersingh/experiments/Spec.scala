package org.sameersingh.experiments

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import io.Source
import java.io.PrintWriter

/**
 * Configuration of the experiments, the different kinds of data that can be stored
 * @author sameer
 * @date 10/6/12
 */

class Spec {
  // all the columns for this experiment
  val columns: ArrayBuffer[Column] = new ArrayBuffer
  protected val shortNameMap: HashMap[String, Int] = new HashMap

  def +=(col: Column): Unit = {
    columns += col
    assert(!shortNameMap.contains(col.shortName))
    shortNameMap(col.shortName) = columns.length - 1
  }

  def addIntColumn(shortName: String, fullName: String) = {
    this += new IntColumn(shortName, fullName)
  }

  def addDoubleColumn(shortName: String, fullName: String) = {
    this += new DoubleColumn(shortName, fullName)
  }

  def addStringColumn(shortName: String, fullName: String) = {
    this += new StringColumn(shortName, fullName)
  }

  def addBooleanColumn(shortName: String, fullName: String) = {
    this += new BooleanColumn(shortName, fullName)
  }

  def addCategoricalColumn(shortName: String, fullName: String, values: Seq[String]) = {
    this += new CategoricalColumn(shortName, fullName, values)
  }

  def getId(shortName: String) = shortNameMap(shortName)

  def apply(shortName: String): Column = apply(shortNameMap(shortName))

  def apply(colId: Int): Column = columns(colId)

  def fromFile(filename: String) {
    for (line <- Source.fromFile(filename).getLines())
      this += Column.readFromLine(line)
  }

  def toFile(filename: String) {
    val writer = new PrintWriter(filename)
    for (col <- columns) {
      writer.println(col.toLine)
    }
    writer.flush
    writer.close
  }
}

object ValueType extends Enumeration {
  type Type = Value
  val Integer, Double, String, Boolean, Categorical = Value
}

abstract class Column {
  def shortName: String

  def fullName: String

  def valueType: ValueType.Type

  def defaultValue: Any

  def valueToDouble(value: Any): Double

  def valueToString(value: Any): String

  def valueFromString(str: String): Any

  def toLine: String = "%s\t%s\t%s".format(valueType, shortName, fullName)
}

object Column {
  def readFromLine(string: String): Column = {
    val split = string.split("\t")
    val shortName = split(1)
    val fullName = split(2)
    ValueType.withName(split(0)) match {
      case ValueType.Integer =>
        assert(split.length == 3)
        new IntColumn(shortName, fullName)
      case ValueType.Double =>
        assert(split.length == 3)
        new DoubleColumn(shortName, fullName)
      case ValueType.String =>
        assert(split.length == 3)
        new StringColumn(shortName, fullName)
      case ValueType.Boolean =>
        assert(split.length == 3)
        new BooleanColumn(shortName, fullName)
      case ValueType.Categorical =>
        assert(split.length >= 4)
        new CategoricalColumn(shortName, fullName, split.drop(3).toSeq) // rest are values
    }
  }
}

class IntColumn(val shortName: String, val fullName: String) extends Column {
  def valueToString(value: Any) = value match {
    case i: Int => i.toString()
  }

  def valueFromString(str: String) = str.toInt

  def valueToDouble(value: Any) = value match {
    case i: Int => i.toDouble
  }

  def valueType = ValueType.Integer

  def defaultValue = 0
}

class DoubleColumn(val shortName: String, val fullName: String) extends Column {
  def valueToString(value: Any) = value match {
    case d: Double => d.toString()
  }

  def valueFromString(str: String) = str.toDouble

  def valueToDouble(value: Any) = value match {
    case d: Double => d
  }

  def valueType = ValueType.Double

  def defaultValue = 0.0
}

class StringColumn(val shortName: String, val fullName: String) extends Column {
  System.err.println("WARNING(unchecked): String columns should not contain tabs, new lines and colons (:).")

  def valueToString(value: Any) = value match {
    case str: String => str
  }

  def valueToDouble(value: Any) = throw new Error("not implemented")

  def valueFromString(str: String) = str

  def valueType = ValueType.String

  def defaultValue = ""

}

class BooleanColumn(val shortName: String, val fullName: String) extends Column {
  def valueToString(value: Any) = value match {
    case b: Boolean => b.toString
  }

  def valueToDouble(value: Any) = value match {
    case b: Boolean => if (b) 1.0 else 0.0
  }

  def valueFromString(str: String) = str.toBoolean

  def valueType = ValueType.Boolean

  def defaultValue = false
}

class CategoricalColumn(val shortName: String, val fullName: String, val values: Seq[String]) extends Column {
  System.err.println("WARNING(unchecked): Categorical values should not contain tabs, new lines and colons (:).")

  val map: HashMap[String, Int] = new HashMap()
  map ++= values.zipWithIndex

  def valueToString(value: Any) = value match {
    case s: String => s
    case i: Int => values(i)
  }

  def valueToDouble(value: Any) = value match {
    case s: String => map(s).toDouble
    case i: Int => i.toDouble
  }

  def valueFromString(str: String) = values(map(str)) // checks whether the value exists automatically

  def valueType = ValueType.Categorical

  def defaultValue = values.head

  override def toLine = "%s\t%s".format(super.toLine, values.mkString("\t"))
}