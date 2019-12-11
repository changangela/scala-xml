/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2019, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package xml
package parsing

import scala.collection.Seq
import org.xml.sax.Attributes
import org.xml.sax.helpers.DefaultHandler

// can be mixed into FactoryAdapter if desired
trait ConsoleErrorHandler extends DefaultHandler {
  // ignore warning, crimson warns even for entity resolution!
  override def warning(ex: SAXParseException | Null): Unit = {}
  override def error(ex: SAXParseException | Null): Unit = printError("Error", ex.nn)
  override def fatalError(ex: SAXParseException | Null): Unit = printError("Fatal Error", ex.nn)

  protected def printError(errtype: String, ex: SAXParseException): Unit =
    Console.withOut(Console.err) {
      val s = "[%s]:%d:%d: %s".format(
        errtype, ex.getLineNumber, ex.getColumnNumber, ex.getMessage)
      Console.println(s)
      Console.flush()
    }
}

/**
 * SAX adapter class, for use with Java SAX parser. Keeps track of
 *  namespace bindings, without relying on namespace handling of the
 *  underlying SAX parser.
 */
abstract class FactoryAdapter extends DefaultHandler with factory.XMLLoader[Node] {
  var rootElem: Node | Null = null

  val buffer = new StringBuilder()
  /** List of attributes
    *
    * Previously was a mutable [[scala.collection.mutable.Stack Stack]], but is now a mutable reference to an immutable [[scala.collection.immutable.List List]].
    *
    * @since 2.0.0
    */
  var attribStack = List.empty[MetaData]
  /** List of elements
    *
    * Previously was a mutable [[scala.collection.mutable.Stack Stack]], but is now a mutable reference to an immutable [[scala.collection.immutable.List List]].
    *
    * @since 2.0.0
    */
  var hStack = List.empty[Node | Null] // [ element ] contains siblings
  /** List of element names
    *
    * Previously was a mutable [[scala.collection.mutable.Stack Stack]], but is now a mutable reference to an immutable [[scala.collection.immutable.List List]].
    *
    * @since 2.0.0
    */
  var tagStack = List.empty[String | Null]
  /** List of namespaces
    *
    * Previously was a mutable [[scala.collection.mutable.Stack Stack]], but is now a mutable reference to an immutable [[scala.collection.immutable.List List]].
    *
    * @since 2.0.0
    */
  var scopeStack = List.empty[NamespaceBinding]

  var curTag: String | Null = null
  var capture: Boolean = false

  // abstract methods

  /**
   * Tests if an XML element contains text.
   * @return true if element named `localName` contains text.
   */
  def nodeContainsText(localName: String): Boolean // abstract

  /**
   * creates an new non-text(tree) node.
   * @param elemName
   * @param attribs
   * @param chIter
   * @return a new XML element.
   */
  def createNode(pre: String | Null, elemName: String | Null, attribs: MetaData,
                 scope: NamespaceBinding | Null, chIter: List[Node]): Node // abstract

  /**
   * creates a Text node.
   * @param text
   * @return a new Text node.
   */
  def createText(text: String): Text // abstract

  /**
   * creates a new processing instruction node.
   */
  def createProcInstr(target: String, data: String): Seq[ProcInstr]

  //
  // ContentHandler methods
  //

  val normalizeWhitespace = false

  /**
   * Characters.
   * @param ch
   * @param offset
   * @param length
   */
  override def characters(ch: Array[Char] | Null, offset: Int, length: Int): Unit = {
    if (!capture) return
    // compliant: report every character
    else if (!normalizeWhitespace) buffer.appendAll(ch.nn, offset, length)
    // normalizing whitespace is not compliant, but useful
    else {
      var it = ch.nn.slice(offset, offset + length).iterator
      while (it.hasNext) {
        val c = it.next()
        val isSpace = c.isWhitespace
        buffer append (if (isSpace) ' ' else c)
        if (isSpace)
          it = it dropWhile (_.isWhitespace)
      }
    }
  }

  private def splitName(s: String) = {
    val idx = s indexOf ':'
    if (idx < 0) (null, s)
    else (s take idx, s drop (idx + 1))
  }

  /* ContentHandler methods */

  /* Start element. */
  override def startElement(
    uri: String | Null,
    _localName: String | Null,
    qname: String | Null,
    attributes: Attributes | Null): Unit =
    {
      captureText()
      tagStack = curTag :: tagStack
      curTag = qname

      val localName = splitName(qname.nn)._2
      capture = nodeContainsText(localName)

      hStack =  null :: hStack
      var m: MetaData = Null
      var scpe: NamespaceBinding =
        if (scopeStack.isEmpty) TopScope
        else scopeStack.head

      for (i <- (0 until attributes.nn.getLength).reverse) {
        val qname = attributes.nn getQName i
        val value = attributes.nn getValue i
        val (pre, key) = splitName(qname)
        def nullIfEmpty(s: String) = if (s == "") null else s

        if ((pre != null && pre.nn == "xmlns") || (pre == null && qname == "xmlns")) {
          val arg = if (pre == null) null else key
          scpe = new NamespaceBinding(arg, nullIfEmpty(value), scpe)
        } else
          m = Attribute(pre, key, Text(value), m)
      }

      scopeStack = scpe :: scopeStack
      attribStack =  m :: attribStack
    }

  /**
   * captures text, possibly normalizing whitespace
   */
  def captureText(): Unit = {
    if (capture && buffer.length > 0)
      hStack = createText(buffer.toString) :: hStack

    buffer.clear()
  }

  /**
   * End element.
   * @param uri
   * @param _localName
   * @param qname
   * @throws org.xml.sax.SAXException if ..
   */
  override def endElement(uri: String | Null, _localName: String | Null, qname: String | Null): Unit = {
    captureText()
    val metaData = attribStack.head
    attribStack = attribStack.tail

    // reverse order to get it right
    val v = hStack.takeWhile(_ != null).reverse
    hStack = hStack.dropWhile(_ != null) match {
      case null :: hs => hs
      case hs => hs
    }
    val (pre, localName) = splitName(qname.nn)
    val scp = scopeStack.head
    scopeStack = scopeStack.tail

    // create element
    rootElem = createNode(pre, localName, metaData, scp, v.map(_.nn))
    hStack = rootElem :: hStack
    curTag = tagStack.head
    tagStack = tagStack.tail
    capture = curTag != null && nodeContainsText(curTag.nn) // root level
  }

  /**
   * Processing instruction.
   */
  override def processingInstruction(target: String | Null, data: String | Null): Unit = {
    captureText()
    hStack = hStack.reverse_:::(createProcInstr(target.nn, data.nn).toList)
  }
}
