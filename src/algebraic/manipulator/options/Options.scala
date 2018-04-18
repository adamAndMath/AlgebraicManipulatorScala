package algebraic.manipulator.options

object Options {
  private var options = Map.empty[String, List[String] => Unit]

  def +=(name: String, args: Int, handler: List[String] => Unit): Unit =
    +=(name, (args) => if (args.length == 1) handler(args) else throw new IllegalArgumentException(s"$name needs 1 argument not ${args.length}"))

  def +=(name: String, handler: List[String] => Unit): Unit =
    if (options contains name)
      throw new IllegalArgumentException(s"An option by the name of $name is defined twice")
    else
      options += name -> handler

  def apply(name: String): List[String] => Unit =
    options.getOrElse(name, throw new IllegalArgumentException(s"Unknown option: $name"))

  def read(args: List[String]): Unit = args match {
    case Nil =>
    case a :: l =>
      if (!a.startsWith("-"))
        throw new IllegalArgumentException(s"$a is not an option")
      val as = l.takeWhile(!_.startsWith("-"))
      apply(a.substring(1))(as)
      read(l.drop(as.length))
  }

  def bool(arg: String): Boolean = arg match {
    case "y" | "t" | "yes" | "true" => true
    case "n" | "f" | "no" | "false" => false
    case _ => throw new IllegalArgumentException(s"Cannot convert $arg to a boolean")
  }
}
