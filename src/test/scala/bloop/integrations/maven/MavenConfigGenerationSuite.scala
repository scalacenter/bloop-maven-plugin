package bloop.integrations.maven

import junit.framework.TestCase
import org.apache.maven.it.util.ResourceExtractor


class MavenConfigGenerationSuite {

  def test(name: String) = {
    val x = ResourceExtractor.simpleExtractResources(getClass(), name)
  }

  @Test
  def basicScala3() = {
    assert(1 == 1)
  }

}
