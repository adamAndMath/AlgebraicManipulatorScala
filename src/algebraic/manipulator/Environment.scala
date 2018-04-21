package algebraic.manipulator

import scala.collection.mutable

trait Environment extends Element {
  val path: List[String]
  def apply(name: String): Element
  def contains(name: String): Boolean
  def find(path: List[String]): Element
  def toFull(path: List[String]): List[String]

  def apply(path: List[String]): Element =
    if (path.tail.isEmpty) apply(path.head)
    else apply(path.head) match {
      case env: Environment => env(path.tail)
      case _ => throw new IllegalArgumentException(s"${path.head} isn't an environment in ${this.path.mkString(".")}")
    }

  def scope(name: String): Environment.Scope = new Environment.Scope(path :+ name, this)
  def bind(params: List[Definition]): Environment = bind(params.map(_.name).toSet)
  def bind(bound: Set[String]): Environment = Environment.SimpleEnvironment(this, bound)
  def +(e: (String, Element)): Environment = Environment.Compound(this, e._1, e._2)
}

object Environment {
  case object empty extends Environment {
    override val path: List[String] = Nil
    override def apply(name: String): Element = throw new IllegalStateException
    override def contains(name: String): Boolean = throw new IllegalStateException
    override def find(path: List[String]): Element = throw new IllegalArgumentException("No such path")
    override def toFull(path: List[String]): List[String] = throw new IllegalArgumentException("No such path")
    override def filter(predicate: Element => Boolean): TraversableOnce[Element] = None
    override def dependencies: Set[String] = Set.empty
    override def validate(env: Environment): Traversable[(List[String], String)] = throw new IllegalStateException
  }

  case class Compound(env: Environment, key: String, element: Element) extends Environment {
    override val path: List[String] = env.path
    override def apply(name: String): Element = throw new IllegalStateException
    override def contains(name: String): Boolean = throw new IllegalStateException

    override def find(path: List[String]): Element =
      if(path.head != key) env.find(path)
      else if (path.tail.isEmpty) element
      else element match {
        case env: Environment => env(path.tail)
        case _ => throw new IllegalArgumentException
      }

    override def toFull(path: List[String]): List[String] =
      if (path.head == key) this.path ++ path
      else env.toFull(path)

    override def filter(predicate: Element => Boolean): TraversableOnce[Element] = throw new IllegalStateException

    override def dependencies: Set[String] = Set.empty
    override def validate(env: Environment): Traversable[(List[String], String)] = element.validate(env)
  }

  case class SimpleEnvironment(env: Environment, bound: Set[String]) extends Environment {
    override val path: List[String] = env.path
    override def apply(path: String): Element = throw new IllegalStateException
    override def contains(name: String): Boolean = bound.contains(name)

    override def find(path: List[String]): Element =
      if (contains(path.head)) apply(path)
      else env.find(path)

    override def toFull(path: List[String]): List[String] =
      if (bound.contains(path.head)) throw new IllegalArgumentException
      else env.toFull(path)

    override def dependencies: Set[String] = Set.empty
    override def validate(env: Environment): Traversable[(List[String], String)] = throw new IllegalStateException
  }

  class Scope(override val path: List[String], val parent: Environment) extends Environment {
    private val elementNames = new mutable.MutableList[String]
    private var elements: Map[String, Element] = Map.empty
    def names: List[String] = elementNames.toList

    override def apply(name: String): Element =
      elements.getOrElse(name, throw new IllegalArgumentException(s"$name not defined in ${path.mkString(".")}"))

    override def contains(name: String): Boolean = elements.contains(name)

    override def find(path: List[String]): Element =
      if (contains(path.head)) apply(path)
      else parent.find(path)

    override def toFull(path: List[String]): List[String] =
      if (elements.contains(path.head)) this.path ++ path
      else parent.toFull(path)

    override def filter(predicate: Element => Boolean): TraversableOnce[Element] =
      if (predicate(this)) Some(this)
      else elementNames.map(elements).filter(predicate)

    override def validate(env: Environment): Traversable[(List[String], String)] =
      elements.flatMap{case (k,e) => e.validate(this).map{case (p, err) => (k::p) -> err}}

    override def dependencies: Set[String] = elements.values.flatMap(_.dependencies).toSet -- elementNames

    def +=(e: (String, Element)): Unit = {
      if (elements.contains(e._1))
        throw new IllegalArgumentException(s"The scope already contains an element named ${e._1}")
      elementNames += e._1
      elements += e
    }

    override def scope(name: String): Scope = {
      val scope = super.scope(name)
      +=(name -> scope)
      scope
    }
  }
}