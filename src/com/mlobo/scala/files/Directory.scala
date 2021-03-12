package com.mlobo.scala.files

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class Directory(
    override val parentPath: String,
    override val name: String,
    val contents: List[DirEntry]
) extends DirEntry(parentPath, name) {
  def fullpath: String = s"$parentPath${Directory.SEPARATOR}$name"

  def hasEntry(name: String): Boolean =
    contents.exists((entry: DirEntry) => entry.name.equals(name))

  def addEntryWithPath(newEntry: DirEntry, dirPath: String): Try[Directory] = {
    @tailrec
    def listFullDirectoriesPath(
        currentDir: Directory,
        path: List[String],
        fullEntriesPath: List[Directory]
    ): Try[List[Directory]] = {
      path match {
        case Nil | "." :: Nil => Success(this :: fullEntriesPath)
        case ::(nextDir, pathTail) =>
          val nextPath: Option[DirEntry] =
            currentDir.contents.find(_.name.equals(nextDir))
          nextPath match {
            case Some(directory: Directory) =>
              listFullDirectoriesPath(
                directory,
                pathTail,
                currentDir :: fullEntriesPath
              )
            case Some(entry: DirEntry) =>
              Failure(new RuntimeException(s"${entry.name} is not a directory"))
            case None =>
              Failure(
                new RuntimeException(s"$nextDir: no such directory/entry.")
              )
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

    val pathList: List[String] =
      dirPath.split("/").filter(_.nonEmpty).init.toList
    val reversedListOfDirsAttempt: Try[List[Directory]] =
      listFullDirectoriesPath(this, pathList, List())

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
    val maybeEntry = contents.find(_.name == newEntry.name)
    maybeEntry match {
      case Some(e) if e.getClass.equals(newEntry.getClass) =>
        val newContents = newEntry :: contents.filterNot(e.equals)
        Success(Directory(parentPath, name, newContents))
      case Some(e) =>
        Failure(
          new RuntimeException(
            s"You are trying to replace a ${e.getClass} with a ${newEntry.getClass}"
          )
        )
      case None =>
        Failure(new RuntimeException(s"Entry ${newEntry.name} does not exist"))
    }
  }
}

object Directory {
  val SEPARATOR = "/"
  val ROOT_PATH = "/"

  def ROOT: Directory = Directory.empty("", "")

  def empty(parentPath: String, name: String): Directory =
    Directory(parentPath, name, List())

  def apply(
      parentPath: String,
      name: String,
      contents: List[DirEntry]
  ): Directory =
    new Directory(parentPath, name, contents)
}
