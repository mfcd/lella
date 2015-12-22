package MultiDictionary

abstract class MultiDictionary
case class Node[T](value: T) extends MultiDictionary
case class MDMap[Key, T](m: Map[Key,MultiDictionary]) extends MultiDictionary {
  def get(k: Key) = m get k

  def adjust(k: Key)(f: MultiDictionary => MDMap[Key, T]) = MDMap(m.updated(k, f(m(k))))

  def nestMap[K](f: T => K) =
    MDMap[Key,T](
      m map { case (k, v) =>
        val value =
          (v: MultiDictionary) match {
            case l:MDList[T] => l toMap f
            case _ => v
          }
        (k, value)
      }
    )
}


case class MDList[T](vals: List[Node[T]]) extends MultiDictionary
{
  def toMap[K](f: T => K) = {
    MDMap[K,T](
      vals
        .groupBy( (x: Node[T]) => f(x.value) )
        .map { case (k,v) => (k, MDList(v)) }
    )
  }
}