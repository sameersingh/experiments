package sameersingh.experiments

import org.junit._
import Assert._
import rules.TemporaryFolder

/**
 * @author sameer
 * @date 10/6/12
 */
@Test
class SpecTest {

  val spec = {
    val s = new Spec
    s.addIntColumn("id", "Id")
    s.addIntColumn("run", "Run")
    s.addStringColumn("expType", "Experiment Type")
    s.addDoubleColumn("score", "Score")
    s
  }

  @Rule
  def tempFolder = new TemporaryFolder()

  @Test
  def testWrite() = {
    val testFile = tempFolder.newFile("test.spec")
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