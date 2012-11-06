package org.sameersingh.experiments

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import io.Source
import java.io.PrintWriter
import collection.mutable

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
    val col = new CategoricalColumn(shortName, fullName)
    values.foreach(v => col.getInt(v))
    this += col
  }

  def addEnumColumn(shortName: String, fullName: String, enum: Enumeration) = {
    this += new EnumerationColumn(shortName, fullName, enum)
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
  val Integer, Double, String, Boolean, Categorical, Enumeration = Value
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
    val remaining = split.drop(3)
    ValueType.withName(split(0)) match {
      case ValueType.Integer =>
        assert(remaining.length == 0)
        new IntColumn(shortName, fullName)
      case ValueType.Double =>
        assert(remaining.length == 0)
        new DoubleColumn(shortName, fullName)
      case ValueType.String =>
        assert(remaining.length == 0)
        new StringColumn(shortName, fullName)
      case ValueType.Boolean =>
        assert(remaining.length == 0)
        new BooleanColumn(shortName, fullName)
      case ValueType.Categorical =>
        assert(remaining.length >= 1)
        val col = new CategoricalColumn(shortName, fullName)
        for (value <- remaining.toSeq)
          col.getInt(value)
        col
      case ValueType.Enumeration =>
        assert(remaining.length >= 1)
        val enum = new Enumeration() {
          remaining.foreach(s => Value(s))
        }
        new EnumerationColumn(shortName, fullName, enum)
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

class CategoricalColumn(val shortName: String, val fullName: String) extends Column {
  System.err.println("WARNING(unchecked): Categorical values should not contain tabs, new lines and colons (:).")
  val values: mutable.Buffer[String] = new ArrayBuffer
  val map: HashMap[String, Int] = new HashMap()

  def getInt(str: String): Int = {
    if (map.contains(str)) map(str)
    else {
      values += str
      map(str) = values.length - 1
      values.length - 1
    }
  }

  def valueToString(value: Any) = value match {
    case s: String => getInt(s).toString
    case i: Int => i.toString
  }

  def valueToDouble(value: Any) = value match {
    case s: String => getInt(s).toDouble
    case i: Int => i.toDouble
  }

  def valueFromString(str: String) = values(str.toInt) // adds it if its not there

  def valueType = ValueType.Categorical

  def defaultValue = 0

  override def toLine = "%s\t%s".format(super.toLine, values.mkString("\t"))
}

class EnumerationColumn(val shortName: String, val fullName: String, val enum: Enumeration) extends Column {
  def valueType = ValueType.Enumeration

  def defaultValue = enum.apply(0)

  def valueToDouble(value: Any) = value match {
    case v: enum.Value => v.id.toDouble
  }

  def valueToString(value: Any) = value match {
    case v: enum.Value => v.id.toString
  }

  def valueFromString(str: String) = enum(str.toInt)

  override def toLine = "%s\t%s".format(super.toLine, enum.values.mkString("\t"))
}