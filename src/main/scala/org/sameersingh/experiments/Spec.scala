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
  val Integer, Double, String = Value
}

abstract class Column {
  def shortName: String

  def fullName: String

  def valueType: ValueType.Type

  def defaultValue: Any

  def valueToString(value: Any): String

  def valueFromString(str: String): Any

  def toLine: String = "%s\t%s\t%s".format(valueType, shortName, fullName)
}

object Column {
  def readFromLine(string: String): Column = {
    val split = string.split("\t")
    assert(split.length == 3)
    val shortName = split(1)
    val fullName = split(2)
    ValueType.withName(split(0)) match {
      case ValueType.Integer => new IntColumn(shortName, fullName)
      case ValueType.Double => new DoubleColumn(shortName, fullName)
      case ValueType.String => new StringColumn(shortName, fullName)
    }
  }
}

class IntColumn(val shortName: String, val fullName: String) extends Column {
  def valueToString(value: Any) = value match {
    case i: Int => i.toString()
  }

  def valueFromString(str: String) = str.toInt

  def valueType = ValueType.Integer

  def defaultValue = 0
}

class DoubleColumn(val shortName: String, val fullName: String) extends Column {
  def valueToString(value: Any) = value match {
    case d: Double => d.toString()
  }

  def valueFromString(str: String) = str.toDouble

  def valueType = ValueType.Double

  def defaultValue = 0.0
}

class StringColumn(val shortName: String, val fullName: String) extends Column {
  System.err.println("WARNING: String columns should not contain tabs, new lines and colons (:).")

  def valueToString(value: Any) = value match {
    case str: String => str
  }

  def valueFromString(str: String) = str

  def valueType = ValueType.String

  def defaultValue = ""

}