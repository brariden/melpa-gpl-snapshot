package chee.cli

import com.typesafe.config.Config
import chee.conf._
import chee.Collection
import chee.properties.Patterns._
import CollectionShow.Opts

class CollectionShow extends ScoptCommand {

  val name = "show"

  type T = Opts

  val defaults = Opts()

  val parser = new Parser {
    opt[String]('p', "pattern") optional() action { (p, c) =>
      c.copy(pattern = p)
    } text ("The format pattern.")

    arg[String]("<name>") optional() action { (n, c) =>
      c.copy(name = n)
    } textW ("The collection name or enough of it to uniquely identify a collection.")
  }

  lazy val detailPattern = seq(
    raw("Name: "), lookup('name), newline,
    raw("Titel: "), lookup('title), newline,
    raw("Query: "), lookup('query), newline,
    raw("Description: "), lookup('description), newline,
    newline
  )

  lazy val onelinePattern =
    seq(lookup('name), raw(" - "), lookup('title), newline)

  def getPattern(cfg: Config, opts: Opts): Either[String, Pattern] =
    opts.pattern match {
      case "oneline" => Right(onelinePattern)
      case "detail" => Right(detailPattern)
      case f => cfg.getFormat(Some(f), "")
    }

  def exec(cfg: Config, opts: Opts): Unit = {
    val colls = cfg.getCollectionConf.list.get
      .filter(_.name.startsWith(opts.name))
      .sortBy(_.name)
    val pattern = getPattern(cfg, opts) match {
      case Right(p) => p
      case Left(err) => chee.UserError(err)
    }
    colls.foreach { coll =>
      out(pattern.right(userError).result(coll))
    }
  }
}

object CollectionShow {
  case class Opts(
    pattern: String = "oneline",
    name: String = "")
}
