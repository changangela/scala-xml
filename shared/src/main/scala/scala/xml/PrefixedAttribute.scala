/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2019, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package xml

import scala.collection.Seq

/**
 * prefixed attributes always have a non-null namespace.
 *
 *  @param pre
 *  @param key
 *  @param value the attribute value
 *  @param next1
 */
class PrefixedAttribute(
  val pre: String | Null,
  val key: String | Null,
  val value: Seq[Node] | Null,
  val next1: MetaData)
  extends Attribute {
  val next = if (value ne null) next1 else next1.remove(key)

  /** same as this(pre, key, Text(value), next), or no attribute if value is null */
  def this(pre: String, key: String, value: String | Null, next: MetaData) =
    this(pre, key, if (value ne null) Text(value.nn) else null: NodeSeq | Null, next)

  /** same as this(pre, key, value.get, next), or no attribute if value is None */
  def this(pre: String, key: String, value: Option[Seq[Node]], next: MetaData) =
    this(pre, key, value.orNull, next)

  /**
   * Returns a copy of this unprefixed attribute with the given
   *  next field.
   */
  def copy(next: MetaData) =
    new PrefixedAttribute(pre, key, value, next)

  def getNamespace(owner: Node) =
    owner.getNamespace(pre).nn

  /** forwards the call to next (because caller looks for unprefixed attribute */
  def apply(key: String | Null): Seq[Node] | Null = next(key.nn)

  /**
   * gets attribute value of qualified (prefixed) attribute with given key
   */
  def apply(namespace: String | Null, scope: NamespaceBinding, key: String | Null): Seq[Node] | Null = {
    if (key == this.key && scope.getURI(pre) == namespace)
      value
    else
      next(namespace, scope, key)
  }
}

object PrefixedAttribute {
  def unapply(x: PrefixedAttribute) = Some((x.pre, x.key, x.value, x.next))
}
