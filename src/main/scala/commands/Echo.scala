package com.mlobo
package commands
import files.{Directory, File}
import filesystem.State
import utils.Path
import utils.Path.SEPARATOR

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class Echo(args: Array[String]) extends Command {

  def createContent(args: Array[String], topIndex: Int): String = {
    @tailrec
    def aux(currentIdx: Int, finalStr: String): String =
      if (currentIdx < topIndex)
        aux(currentIdx + 1, finalStr + ' ' + args(currentIdx))
      else finalStr

    aux(0, "")
  }

  def getRootAfterEcho(
      state: State,
      path: Path,
      contents: String,
      append: Boolean
  ): State = {
    val resultFile =
      state.root.getEntryWithPath(path.getParent, path.getLast)
    resultFile match {
      case Failure(_) =>
        state.addEntryToDirectoryTree(
          new File(path.getParent, path.getLast, contents)
        )
      case Success(file: File) =>
        val newFile = file.setContent(
          if (append) file.content + ' ' + contents else contents
        )
        state.addEntryToDirectoryTree(newFile, update = true)
      case _ => state.setMessage("Not a file!")
    }
  }

  def doEcho(
      state: State,
      contents: String,
      filename: String,
      append: Boolean
  ): State =
    if (filename.contains(SEPARATOR))
      state.setMessage("Filename must not contain separators")
    else {
      getRootAfterEcho(
        state,
        state.wd.fullpath.join(filename),
        contents,
        append
      )
    }

  def apply(state: State): State =
    args match {
      case Array()          => state
      case Array(singleArg) => state.setMessage(singleArg)
      case _ =>
        val operator = args(args.length - 2)
        val filename = args(args.length - 1)
        val contents = createContent(args, args.length - 2)

        operator match {
          case ">>" => doEcho(state, contents, filename, append = true)
          case ">"  => doEcho(state, contents, filename, append = false)
          case _    => state.setMessage(createContent(args, args.length))
        }
    }
}
