package com.mlobo
package commands

import filesystem.State

trait Command {
  def apply(state: State): State
}

object Command {
  val MKDIR = "mkdir"
  val LS = "ls"
  val CD = "cd"
  val PWD = "pwd"
  val TOUCH = "touch"

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
      case t @ Array(LS, _*) =>
        t match {
          case Array(_)     => new Ls
          case Array(_, _*) => wrongNumberOfArgs(0)
        }
      case t @ Array(CD, _*) =>
        t match {
          case Array(_)       => incompleteCommand(CD)
          case Array(_, name) => new Cd(name)
          case _              => wrongNumberOfArgs(1)
        }
      case t @ Array(PWD, _*) =>
        t match {
          case Array(_)     => new Pwd
          case Array(_, _*) => wrongNumberOfArgs(0)
        }
      case t @ Array(TOUCH, _*) =>
        t match {
          case Array(_)       => incompleteCommand(TOUCH)
          case Array(_, name) => new Touch(name)
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
