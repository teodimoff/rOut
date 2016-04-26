package io.rout.path


abstract class Path { self =>

  def depth: Int = self.toList.size
  def drop(n: Int) = Path(self.toList.drop(n))
  def /(child: String) =  new /(self, child)
  def /[A](path: Path => MatchPath[A])(implicit c: PathMatcher[A]) =  MatchPath[A](self).as[A]
  def toList: List[String]
  def parent: Path
  def lastOption: Option[String]
  def startsWith(other: Path): Boolean

  def compare(other: Path) = Path.compare(self,other)

  def comparable = (path:Path) => compare(path)

  def as[A](implicit c: PathMatcher[A]): Path => Option[(A,Path)] =
    (path: Path) =>  Path.nonStrict(path,self,depth) match {
      case true =>  path.toList.lift(self.depth).flatMap(s =>
        try c(s).map(a=> (a,path.drop(depth + 1))) catch { case t: Throwable => None})
      case false => None
    }

  //match head
  def asCur[A](implicit c: PathMatcher[A]):  Path => Option[(A,Path)] =
    (path: Path) => path.toList.headOption.flatMap(s =>
      try c(s).map(a=> (a,path.drop(1))) catch { case t: Throwable => None})
}

case class MatchPath[A](parent: Path) extends Path {
  lazy val toList: List[String] = parent.toList
  def lastOption: Option[String] = parent.lastOption
  lazy val asString = parent.toString
  override def toString = asString
  def startsWith(other: Path) = {
    val components = other.toList
    (toList take components.length) == components
  }

}

object Path {
  def apply(str: String): Path =
    if (str == "" || str == "/")
      Root
    else if (!str.startsWith("/"))
      Path("/" + str)
    else {
      val slash = str.lastIndexOf('/')
      val prefix = Path(str.substring(0, slash))
      if (slash == str.length - 1)
        prefix
      else
        prefix / str.substring(slash + 1)
    }

  def apply(first: String, rest: String*): Path =
    rest.foldLeft(Root / first)( _ / _)

  def apply(list: List[String]): Path = list.foldLeft(Root: Path)(_ / _)

  def apply[A](path: Path => Option[(A,Path)]): Path => Option[A] = (p:Path) => path(p).flatMap {
    case (a,Root) => Some(a)
    case _ => None
  }

  def unapplySeq(path: Path) = Some(path.toList)

  //compare to path to given depth
  // import com.teodimoff.finagle.http.routing.exp._

  def exact(current: Path,matchTo: Path,toDepth: Int) = {
    def compareh(left:List[String],rigth: List[String],depth: Int): Boolean =
      if(left.size == rigth.size)
        if(depth == 0) true else
          left match {
            case Nil => if(rigth == Nil) true else false
            case lhead :: lxs => rigth match {
              case Nil => false
              case rhead :: rxs =>
                //println("compare",lhead,rhead, lhead == rhead)
                if(lhead == rhead) compareh(lxs,rxs, depth - 1) else false
            }
          } else false
    compareh(current.toList,matchTo.toList,toDepth)
  }

  def nonStrict(current: Path,matchTo: Path,toDepth: Int) = {
    def compareh(left:List[String],rigth: List[String],depth: Int): Boolean =
        if(depth == 0) true else
          left match {
            case Nil => if(rigth == Nil) true else false
            case lhead :: lxs => rigth match {
              case Nil => false
              case rhead :: rxs =>
                //println("compare",lhead,rhead, lhead == rhead)
                if(lhead == rhead) compareh(lxs,rxs, depth - 1) else false
            }
          }
    compareh(current.toList,matchTo.toList,toDepth)
  }

  def compare(current: Path,matchTo: Path) = {
    def compareh(left:List[String],rigth: List[String]): Boolean =
      if(left.size == rigth.size)
          left match {
            case Nil => if(rigth == Nil) true else false
            case lhead :: lxs => rigth match {
              case Nil => false
              case rhead :: rxs =>
                //println("compare",lhead,rhead, lhead == rhead)
                if(lhead == rhead) compareh(lxs,rxs) else false
            }
          } else false
    compareh(current.toList,matchTo.toList)
  }

  def compare(current: Path,matchTo: String): Boolean = compare(current,Path(matchTo))

  implicit class PathOps[A](val pathFun: Path => Option[(A,Path)]) extends AnyVal {
    def /[B](other: Path => MatchPath[B])(implicit c: PathMatcher[B]): Path => Option[((A,B),Path)] =
      (path: Path) => pathFun(path).flatMap(x=> other(x._2).asCur[B].apply(x._2).map(b => ((x._1,b._1),b._2)))
  }

}

/**
 * Path separator extractor:
 *   Path("/1/2/3/test.json") match {
 *     case Root / "1" / "2" / "3" / "test.json" => ...
 */
case class /(parent: Path, child: String) extends Path {
  lazy val toList: List[String] = parent.toList ++ Seq(child)
  def lastOption: Option[String] = Some(child)
  lazy val asString = parent.toString + "/" + child
  override def toString = asString
  def startsWith(other: Path) = {
    val components = other.toList
    (toList take components.length) == components
  }
}

/**
 * Root extractor:
 *   Path("/") match {
 *     case Root => ...
 *   }
 */
case object Root extends Path {
  def toList: List[String] = Nil
  def parent = this
  def lastOption: Option[String] = None
  override def toString = ""
  def startsWith(other: Path) = other == Root
}
