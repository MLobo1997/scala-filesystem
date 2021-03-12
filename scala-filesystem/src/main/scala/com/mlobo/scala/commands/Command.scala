package com.mlobo.scala.commands

import com.mlobo.scala.filesystem.State

trait Command {
  def apply(state: State): State
}

object Command {
  val MKDIR = "mkdir"

  def from(input: String): Command = {
    val tokens: Array[String] = input.split(" ")
    if (input.isEmpty) emptyCommand
    tokens match {
      case t if t.isEmpty => emptyCommand
      case t @ Array(MKDIR, _*) =>
        t match {
          case Array(_)       => incompleteCommand(MKDIR)
          case Array(_, name) => new Mkdir(name)
          case _              => wrongNumberOfArgs(1)
        }
      case _ => new UnknownCommand
    }
  }

  def emptyCommand: Command = (state: State) => state

  def incompleteCommand(name: String): Command =
    (state: State) => state.setMessage(s"$name is missing arguments.")

  def wrongNumberOfArgs(expectedNumber: Int): Command =
    (state: State) =>
      state.setMessage(s"Expected exactly $expectedNumber arguments")

}
