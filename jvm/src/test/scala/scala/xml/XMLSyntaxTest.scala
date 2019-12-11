package scala.xml

import org.junit.Test
import org.junit.Assert.assertEquals

class XMLSyntaxTestJVM {

  @Test
  def test3(): Unit = {
    // this demonstrates how to handle entities
    val s = scala.io.Source.fromString("<a>&nbsp;</a>")
    object parser extends xml.parsing.ConstructingParser(s, false /*ignore ws*/) {
      override def replacementText(entityName: String): scala.io.Source = {
        entityName match {
          case "nbsp" => scala.io.Source.fromString("\u0160");
          case _ => super.replacementText(entityName);
        }
      }
      nextch(); // !!important, to initialize the parser
    }
    val parsed = parser.element(TopScope) // parse the source as element
    // alternatively, we could call document()
    assertEquals("<a>Š</a>", parsed.toString)
  }

}
