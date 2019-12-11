/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2019, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package xml
package include.sax

import org.xml.sax.{ ContentHandler, Locator, Attributes }
import org.xml.sax.ext.LexicalHandler
import java.io.{ OutputStream, OutputStreamWriter, IOException }

/**
 * XIncluder is a SAX `ContentHandler` that writes its XML document onto
 * an output stream after resolving all `xinclude:include` elements.
 *
 * Based on Eliotte Rusty Harold's SAXXIncluder.
 */
class XIncluder(outs: OutputStream, encoding: String) extends ContentHandler with LexicalHandler {

  var out = new OutputStreamWriter(outs, encoding)

  def setDocumentLocator(locator: Locator | Null): Unit = {}

  def startDocument(): Unit = {
    try {
      out.write("<?xml version='1.0' encoding='"
        + encoding + "'?>\r\n")
    } catch {
      case e: IOException =>
        throw new SAXException("Write failed", e)
    }
  }

  def endDocument(): Unit = {
    try {
      out.flush()
    } catch {
      case e: IOException =>
        throw new SAXException("Flush failed", e)
    }
  }

  def startPrefixMapping(prefix: String | Null, uri: String | Null): Unit = {}

  def endPrefixMapping(prefix: String | Null): Unit = {}

  def startElement(namespaceURI: String | Null, localName: String | Null, qualifiedName: String | Null, atts: Attributes | Null) = {
    try {
      out.write("<" + qualifiedName)
      var i = 0; while (i < atts.nn.getLength()) {
        out.write(" ")
        out.write(atts.nn.getQName(i))
        out.write("='")
        val value = atts.nn.getValue(i)
        // @todo Need to use character references if the encoding
        // can't support the character
        out.write(scala.xml.Utility.escape(value))
        out.write("'")
        i += 1
      }
      out.write(">")
    } catch {
      case e: IOException =>
        throw new SAXException("Write failed", e)
    }
  }

  def endElement(namespaceURI: String | Null, localName: String | Null, qualifiedName: String | Null): Unit = {
    try {
      out.write("</" + qualifiedName + ">")
    } catch {
      case e: IOException =>
        throw new SAXException("Write failed", e)
    }
  }

  // need to escape characters that are not in the given
  // encoding using character references????
  def characters(ch: Array[Char] | Null, start: Int, length: Int): Unit = {
    try {
      var i = 0; while (i < length) {
        val c = ch.nn(start + i)
        if (c == '&') out.write("&amp;")
        else if (c == '<') out.write("&lt;")
        // This next fix is normally not necessary.
        // However, it is required if text contains ]]>
        // (The end CDATA section delimiter)
        else if (c == '>') out.write("&gt;")
        else out.write(c.toInt)
        i += 1
      }
    } catch {
      case e: IOException =>
        throw new SAXException("Write failed", e)
    }
  }

  def ignorableWhitespace(ch: Array[Char] | Null, start: Int, length: Int): Unit = {
    this.characters(ch, start, length)
  }

  // do I need to escape text in PI????
  def processingInstruction(target: String | Null, data: String | Null): Unit = {
    try {
      out.write("<?" + target + " " + data + "?>")
    } catch {
      case e: IOException =>
        throw new SAXException("Write failed", e)
    }
  }

  def skippedEntity(name: String | Null): Unit = {
    try {
      out.write("&" + name + ";")
    } catch {
      case e: IOException =>
        throw new SAXException("Write failed", e)
    }
  }

  // LexicalHandler methods
  private var inDTD: Boolean = false
  private var entities = List.empty[String]

  def startDTD(name: String | Null, publicID: String | Null, systemID: String | Null): Unit = {
    inDTD = true
    // if this is the source document, output a DOCTYPE declaration
    if (entities.isEmpty) {
      var id = ""
      if (publicID != null) id = " PUBLIC \"" + publicID.nn + "\" \"" + systemID.nn + '"'
      else if (systemID != null) id = " SYSTEM \"" + systemID.nn + '"'
      try {
        out.write("<!DOCTYPE " + name.nn + id + ">\r\n")
      } catch {
        case e: IOException =>
          throw new SAXException("Error while writing DOCTYPE", e)
      }
    }
  }
  def endDTD(): Unit = {}

  def startEntity(name: String | Null): Unit = {
    entities =  name.nn :: entities
  }

  def endEntity(name: String | Null): Unit = {
    entities = entities.tail
  }

  def startCDATA(): Unit = {}
  def endCDATA(): Unit = {}

  // Just need this reference so we can ask if a comment is
  // inside an include element or not
  private var filter: XIncludeFilter | Null = null

  def setFilter(filter: XIncludeFilter): Unit = {
    this.filter = filter
  }

  def comment(ch: Array[Char] | Null, start: Int, length: Int): Unit = {
    if (!inDTD && !filter.nn.insideIncludeElement()) {
      try {
        out.write("<!--")
        out.write(ch.nn, start, length)
        out.write("-->")
      } catch {
        case e: IOException =>
          throw new SAXException("Write failed", e)
      }
    }
  }
}
