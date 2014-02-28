import java.io.File
import scala.util.parsing.combinator.JavaTokenParsers

object UNIXDSL {

  implicit class RichFile(f: File) {
    def cat = io.Source.fromFile(f).getLines().toSeq
  }

  implicit def str2file(s: String) = new File(s)

  type LINES = Seq[String]

  trait GEN {
    self =>
    def run: LINES

    def |(proc: SPROC): SPROC = new SPROC {
      def run(str: LINES) = proc.run(self.run)
    }
  }

  case class CAT(path: File) extends GEN {
    def run = path.cat
  }

  trait SPROC {
    self =>
    def run(str: LINES): LINES
    def |(proc: SPROC): SPROC = new SPROC {
      def run(lines: LINES) = proc.run(self.run(lines))
    }

    def exec: Unit = run(Seq()).foreach(println)
  }

  case class SED(str: String, replace: String) extends SPROC {
    def run(lines: LINES) = lines.map { _.replaceAll(str, replace) }
  }

  class SORT[A : Ordering](field: Int, conv: String => A) extends SPROC {
    def run(lines: LINES) =
      lines
        .zip(lines.map { _.split("\\t") })
        .sortBy { case (line, split) => conv(split(field-1)) }
        .map { case (line, _) => line }
        .reverse
  }

  object SORT {
    def apply(field: Int, args: String) = args match {
      case "-n" => new SORT(field, _.toInt)
    }

    def apply(field: Int) = new SORT(field, identity(_))
  }

  case class UNIQ() extends SPROC {
    def run(lines: LINES) =
      lines
        .groupBy { s => s }
        .map { case (line, l) => s"$line\t${l.length}" }
        .toSeq
  }

  case class HEAD(n: Int) extends SPROC {
    def run(lines: LINES) = lines.take(n)
  }

  case class EXTRACT(n: Int, sep: String = "\\t") extends SPROC {
    def run(lines: LINES) = lines.map(_.split(sep).apply(n-1))
  }

  object SHELL extends JavaTokenParsers {
    def file = "[A-Za-z0-9/_.]+".r
    def cat = "cat" ~> file ^^ { s => CAT(s) }
    def ls: Parser[GEN] = ???
    def extract = "extract" ~> wholeNumber ^^ { s => EXTRACT(s.toInt) }
    def uniq = "uniq" ^^ { _ => UNIQ() }
    def sort = "sort" ~> (wholeNumber ~ opt("-n")) ^^ {
      case field ~ Some(opt) => SORT(field.toInt, opt.toString)
      case field ~ None      => SORT(field.toInt)
    }
    def head = "head" ~> wholeNumber ^^ { s => HEAD(s.toInt) }

    def gen = cat | ls
    def sproc: Parser[SPROC] = extract | uniq | sort | head
    def pipe = (gen <~ "|") ~ repsep(sproc, "|") ^^ {
      case gen ~ sprocs => sprocs.tail.foldLeft(gen | sprocs.head)(_ | _)
    }

    def apply(expr: String) = parse(pipe, expr) match {
      case Success(proc, _) => proc.exec
      case Failure(msg, _)  => println(msg)
      case Error(msg, _)    => println(msg)
    }
  }
}

import UNIXDSL._

// INTERNAL DSL
(CAT("/tmp/20140226.export.CSV") | EXTRACT(7) | UNIQ() | SORT(2, "-n") | HEAD(10)).exec


// EXTERNAL DSL
SHELL("cat /tmp/20140226.export.CSV | extract 7 | uniq | sort 2 -n | head 10")
