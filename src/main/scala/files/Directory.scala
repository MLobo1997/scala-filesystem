package com.mlobo
package files

import utils.Path

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class Directory(
    override val parentPath: Path,
    override val name: String,
    val contents: List[DirEntry]
) extends DirEntry(parentPath, name) {

  def hasEntry(name: String): Boolean =
    contents.exists((entry: DirEntry) => entry.name.equals(name))

  def getDirectoryWithRelativePath(dir: Directory): Try[Directory] =
    getDirectoryWithRelativePath(dir.parentPath, dir.name)

  def getDirectoryWithRelativePath(
      relativeParentPath: Path,
      name: String
  ): Try[Directory] =
    getEntryWithPath(relativeParentPath, name).flatMap {
      case dir: Directory => Success(dir)
      case entry =>
        Failure(new RuntimeException(s"${entry.name} is not a directory"))
    }

  def getDirectory(name: String): Try[Directory] =
    getEntry(name)
      .flatMap {
        case directory: Directory => Success(directory)
        case _ =>
          Failure(new RuntimeException(s"$name is not a directory"))
      }

  def getEntry(name: String): Try[DirEntry] =
    contents
      .find(_.name == name)
      .map(Success(_))
      .getOrElse(Failure(new RuntimeException(s"$name: no such entry")))

  def getEntryWithPath(
      relativeParentPath: Path,
      name: String
  ): Try[DirEntry] = {
    @tailrec
    def aux(
        maybeCurrentDir: Try[Directory],
        pathList: List[String]
    ): Try[DirEntry] = {
      maybeCurrentDir match {
        case Success(currentDir) =>
          pathList match {
            case Nil =>
              if (name.nonEmpty) currentDir.getEntry(name)
              else Success(currentDir)
            case ::(nextDir, tail) =>
              aux(currentDir.getDirectory(nextDir), tail)
          }
        case fail @ Failure(_) => fail
      }
    }

    aux(Success(this), relativeParentPath.listedPath)
  }

  def addEntryInRelativePath(newEntry: DirEntry): Try[Directory] = {
    @tailrec
    def listFullDirectoriesPath(
        currentDir: Directory,
        path: List[String],
        fullEntriesPath: List[Directory]
    ): Try[List[Directory]] = {
      path match {
        case Nil | "." :: Nil => Success(currentDir :: fullEntriesPath)
        case ::(nextDir, pathTail) =>
          val nextPath: Try[DirEntry] =
            currentDir.getEntry(nextDir)
          nextPath match {
            case Success(directory: Directory) =>
              listFullDirectoriesPath(
                directory,
                pathTail,
                currentDir :: fullEntriesPath
              )
            case Success(entry: DirEntry) =>
              Failure(new RuntimeException(s"${entry.name} is not a directory"))
            case Failure(e) => Failure(e)
          }
      }
    }

    @tailrec
    def updateChainOfDirectories(
        reversedListOfDirs: List[Directory],
        dirAccumulator: Try[Directory]
    ): Try[Directory] =
      (reversedListOfDirs, dirAccumulator) match {
        case (_, Failure(exception)) => Failure(exception)
        case (Nil, _)                => dirAccumulator
        case (::(dir, next), Success(acc)) =>
          updateChainOfDirectories(
            next,
            dir.updateEntry(acc)
          )
      }

    val reversedListOfDirsAttempt: Try[List[Directory]] =
      listFullDirectoriesPath(this, newEntry.parentPath.listedPath, List())

    reversedListOfDirsAttempt.flatMap(reversedListOfDirs =>
      updateChainOfDirectories(
        reversedListOfDirs.tail,
        reversedListOfDirs.head.addNewEntry(newEntry)
      )
    )
  }

  def addNewEntry(newEntry: DirEntry): Try[Directory] =
    if (!contents.exists(_.name == newEntry.name)) {
      Success(withContents(newEntry :: contents))
    } else {
      Failure(new RuntimeException(s"Entry name $name"))
    }

  def withContents(newContents: List[DirEntry]): Directory =
    Directory(parentPath, name, newContents)

  def updateEntry(newEntry: DirEntry): Try[Directory] = {
    val maybeEntry = getEntry(newEntry.name)
    maybeEntry.flatMap {
      case entry if entry.getClass.equals(newEntry.getClass) =>
        val newContents = newEntry :: contents.filterNot(entry.equals)
        Success(Directory(parentPath, name, newContents))
      case entry =>
        Failure(
          new RuntimeException(
            s"You are trying to replace a ${entry.getClass} with a ${newEntry.getClass}"
          )
        )
    }
  }
}

object Directory {
  val SEPARATOR = "/"
  val ROOT_PATH = "/"

  def ROOT: Directory = Directory.empty(Path(""), "")

  def empty(parentPath: Path, name: String): Directory =
    Directory(parentPath, name, List())

  def apply(
      parentPath: Path,
      name: String,
      contents: List[DirEntry]
  ): Directory =
    new Directory(parentPath, name, contents)
}
