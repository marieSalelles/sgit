package sgit.app.commandlineParser

import scopt.{OParser, OParserBuilder}

object Parser {
  val builder: OParserBuilder[Config] = OParser.builder[Config]
  val parser1: OParser[Unit, Config] = {
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
      cmd("log")
          .action((_,c)=> c.copy(command = "log"))
          .text("Show commit logs.")
        .children(
          opt[Unit]("full diff")
            .abbr("p")
            .action((_, c) => c.copy(option = "p"))
            .text("Shows the changes over time."),
          opt[Unit]("stat")
            .action((_, c) => c.copy(option = "stat"))
            .text("Generate a diffstat.")
        ),
      cmd("branch")
          .action((_,c)=>c.copy(command = "branch"))
          .text("Create a new branch.")
          .children(
            arg[String]("branch name")
                .optional
                .action((x, c) => c.copy(branch = x))
                .text("branch name"),
            opt[Unit]("verbose")
              .abbr("v")
              .action((_,c)=> c.copy(option = "v"))
              .text("List all existing branches.")
          ),
      cmd("tag")
          .action((_,c) => c.copy(command = "tag"))
          .text("Create a tag.")
          .children(
            arg[String]("tag name")
              .optional
              .action((x,c)=> c.copy(tag = x))
              .text("tag name")
          ),
      cmd("checkout")
          .action((_,c)=> c.copy(command="checkout"))
          .text("Switch branches or restore working tree files")
      .children(
        arg[String]("Branch or tag or commit hash")
          .action((x,c)=>c.copy(checkout = x))
          .text("Branch or tag or commit hash")
      ),
      cmd("merge")
        .action((_,c)=> c.copy(command="merge"))
        .text("Join two or more development histories together.")
      .children(
        arg[String]("Branch")
          .action((x,c)=>c.copy(merge = x))
          .text("Branch name.")
      ),
      cmd("rebase")
      .action((_,c)=>c.copy(command="rebase"))
      .text("Reapply commits on top of another base tip")
      .children(
        arg[String]("Commit hash or branch name")
          .action((x,c)=> c.copy(rebase = x))
          .text("Commit hash or branch name"),
        opt[Unit]("interactive")
          .abbr("i")
          .action((_,c)=> c.copy(option = "i"))
          .text("Make a list of the commits which are about to be rebased. Let the user edit that list before rebasing.")
      ),
      checkConfig(
        c =>
          if (c.command == "") failure("Write a command.")
          else success)
        )
    }
}