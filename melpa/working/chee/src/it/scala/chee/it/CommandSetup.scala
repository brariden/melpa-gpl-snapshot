package chee.it

import com.typesafe.scalalogging.LazyLogging
import java.util.UUID

import scala.util.{Failure, Try}

import CommandSetup._
import better.files._
import chee.TestInfo
import chee.cli.{Command, LocationAdd}
import chee.conf._
import chee.query.SqliteBackend

trait CommandSetup {

  private def mkDir: File = {
    val name = UUID.randomUUID().toString
    val dir = TestInfo.targetDir / "it" / name
    dir.createDirectories()
    dir
  }

  val globalSetup: File => Directories =
    dir => Directories.inDirectory(dir)

  val repoSetup: File => Directories = { dir =>
    val dirs = Directories.atRepoRoot(dir).copy(userDir = Some(dir))
    dirs.configDir.foreach(_.createDirectories())
    dirs
  }

  val addImages: Setup => Setup = { setup =>
    addLocation(setup.files, setup)
    setup
  }

  private def teardown(setupVals: Setup) = {
    if (setupVals.userDir.exists)
      setupVals.userDir.delete()
  }

  def addLocation(pics: File, setupVals: Setup): Unit = {
    pics.createIfNotExists(asDirectory = true)
    TestInfo.images.foreach(f => f.copyTo(pics / f.name))
    val addCmd = new LocationAdd with BufferOut
    val addOpts = LocationAdd.Opts(dirs = Seq(pics), recursive = true)
    val sqlite = new SqliteBackend(setupVals.cfg)
    addCmd.indexDirs(setupVals.cfg, addOpts, sqlite)
  }

  def cheeSetup(before: File => Setup)(code: Setup => Any): Unit = {
    val setupVals = before(mkDir)
    // assume the tmpdir exists; this is ensured in Main
    setupVals.cfg.getFile("chee.tmpdir").createDirectories()
    val test = Try(code(setupVals))
    teardown(setupVals)
    test match {
      case Failure(ex) => throw ex
      case _ =>
    }
  }

  def globalChee(before: Setup => Setup = identity)(code: Setup => Any): Unit = {
    cheeSetup(globalSetup andThen Setup.apply andThen before)(code)
  }

  def repoChee(before: Setup => Setup = identity)(code: Setup => Any): Unit =
    cheeSetup(repoSetup andThen Setup.apply andThen before)(code)

  def bothChee(before: Setup => Setup = identity)(code: Setup => Any): Unit = {
    globalChee(before)(code)
    repoChee(before)(code)
  }

  implicit class CommandBufferRun(cmd: Command with BufferOut) extends LazyLogging {
    def run(setup: Setup, args: String*): (List[String], List[String]) = {
      logger.info(s"Running command ${cmd.name} with args $args")
      cmd.exec(setup.cfg, args.toArray)
      (cmd.stdoutLines, cmd.stderrLines)
    }
  }
}

object CommandSetup {
  case class Setup(dirs: Directories) {
    val cfg = CheeConf.load(dirs)
    val userDir = dirs.userDir match {
      case Some(dir) => dir
      case _ => sys.error("Invalid test configuration. No user dir set.")
    }
    val files = userDir / "files"
  }
}
