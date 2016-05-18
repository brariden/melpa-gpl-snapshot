package chee.metadata

import chee.util.Render

object RecElement {
  sealed trait Element
  sealed trait RecordEl extends Element
  sealed trait Entry extends Element

  case class Field(label: String, value: String, pos: Int) extends RecordEl
  case class Comment(text: String, pos: Int) extends RecordEl with Entry

  sealed trait Descriptor extends Entry
  object Descriptor {
    sealed trait Type {
      def name: String
      def definition: String
    }

    case object Empty extends Descriptor
    case class NonEmpty(els: Vector[RecordEl], pos: Int) extends Descriptor

    def apply(els: Vector[RecordEl], pos: Int): Descriptor =
      if (els.isEmpty) Empty else NonEmpty(els, pos)

    def apply(field: RecordEl*): Descriptor =
      if (field.isEmpty) Empty else NonEmpty(field.toVector, 0)
  }


  case class Record(els: Vector[RecordEl], descriptor: Descriptor, pos: Int) extends Entry {
    def + (f: RecordEl) = copy(els :+ f)

    lazy val fields: Vector[Field] = els.collect {
      case f: Field => f
    }

    def set(label: String, values: String*): Record =
      values.foldLeft(filter(_.label != label)) { (r, v) =>
        r + Field(label, v, 0)
      }

    def get(label: String): Vector[Field] =
      fields.filter(_.label == label)

    def valueOf(label: String): Vector[String] =
      get(label).map(_.value)

    def valueEq(label: String, test: String): Boolean =
      fields.find(_.value == test).isDefined

    def filter(p: Field => Boolean) =
      copy(els = els filter {
        case f: Field => p(f)
        case _ => true
      })
  }

  object Record {
    def apply(field: RecordEl*): Record =
      Record(field.toVector, Descriptor.Empty, 0)
  }

  object RecordId {
    def unapply(r: Record): Option[(String, Record)] =
      r.valueOf("Checksum").headOption match {
        case Some(c) => Some((c, r))
        case _ => None
      }
  }

  case class Database(els: Vector[Entry]) extends Element {
    def + (r: Entry) = Database(els :+ r)
    def ++ (d: Database) = Database(els ++ d.els)

    lazy val records = els.collect {
      case r: Record => r
    }

    def filter(p: Record => Boolean) =
      Database(els filter {
        case r: Record => p(r)
        case _ => true
      })

    def filterId(p: String => Boolean) = filter {
      case RecordId(id, _) => p(id)
    }
  }

  object Database {
    val Empty = Database(Vector.empty)
  }
}

object RecFormat {
  import RecElement._

  implicit val _comment: Render[Comment] =
    Render { comment =>
      "#" + comment.text.replace("\n", "\n#")
    }

  implicit val _field: Render[Field] =
    Render { field =>
      s"""${field.label}: ${field.value.replace("\n", "\n+ ")}"""
    }

  implicit def _recordEl(implicit rf: Render[Field], rc: Render[Comment]): Render[RecordEl] =
    Render {
      case f: Field => rf.render(f)
      case c: Comment => rc.render(c)
    }

  implicit def _record(implicit re: Render[RecordEl]): Render[Record] =
    Render { record =>
      record.els.map(re.render).mkString("\n")
    }

  implicit def _descriptor(implicit re: Render[RecordEl]): Render[Descriptor] =
    Render {
      case Descriptor.Empty => ""
      case Descriptor.NonEmpty(els, _) => els.map(re.render).mkString("\n")
    }

  implicit def _entry(implicit
    rr: Render[Record],
    rd: Render[Descriptor],
    rc: Render[Comment]): Render[Entry] =
  Render {
    case c: Comment => rc.render(c)
    case r: Record => rr.render(r)
    case d: Descriptor => rd.render(d)
  }

  implicit def _database(implicit re: Render[Entry]): Render[Database] =
    Render { db =>
      db.els.map(re.render).mkString("\n\n")
    }
}
