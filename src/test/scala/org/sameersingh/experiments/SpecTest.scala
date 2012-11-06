package org.sameersingh.experiments

import org.junit._
import Assert._
import rules.TemporaryFolder

/**
 * @author sameer
 * @date 10/6/12
 */
@Test
class SpecTest {

  @Test
  def testWrite(): Unit = {
    val testFile = java.io.File.createTempFile("test", "spec")
    println(testFile.getCanonicalPath)
    val spec = SpecTest.spec
    spec.toFile(testFile.getAbsolutePath)
    val newSpec = new Spec
    newSpec.fromFile(testFile.getAbsolutePath)
    assertEquals(spec.columns.length, newSpec.columns.length)
    for (col <- spec.columns)
      println(col.toLine)
    for (col <- newSpec.columns)
      println(col.toLine)
  }

}

object SpecTest {
  object Enum extends Enumeration {
    val One, Two, Three, Four = Value
  }

  def spec = {
    val s = new Spec
    s.addIntColumn("id", "Id")
    s.addIntColumn("run", "Run")
    s.addStringColumn("expType", "Experiment Type")
    s.addDoubleColumn("score", "Score")
    s.addBooleanColumn("isValid", "Whether the runs were valid or not")
    s.addCategoricalColumn("dataType", "Type of the data used", Seq("train", "test", "dev"))
    s.addEnumColumn("count", "I know how to count!", Enum)
    s
  }
}