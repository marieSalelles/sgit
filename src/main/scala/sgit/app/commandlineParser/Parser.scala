package sgit.app.commandlineParser

import scopt.OParser

object Parser {
  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("sgit"),
      head("SGit", "1.0"),
      cmd("init")
        .action((_, c) => c.copy(command = "init"))
        .text("Create an empty Sgit repository."),
      cmd("status")
        .action((_, c) => c.copy(command = "status"))
        .text("Show the working tree status."),
      cmd("diff")
        .action((_, c) => c.copy(command = "diff"))
        .text("Show changes between commits."),
      cmd("add")
        .action((_, c) => c.copy(command = "add"))
        .text("Add file contents to the index.")
        .children(
          arg[String]("<file>... or regex")
            .unbounded()
            .action((x, c) => c.copy(files = c.files :+ x))
            .text("Several File(s) or a regex.")
        ),
      cmd("commit")
          .action((_,c) => c.copy(command = "commit"))
          .text("Record changes to the repository.")
          .children(
            arg[String]("message")
              .action((x, c) => c.copy(message = x))
              .text("Commit message.")
          ),
        checkConfig(
        c =>
          if (c.command == "") failure("Write a command.")
          else success)
        )
    }
}