package sgit.commitHistory

import better.files._
import org.scalatest.{BeforeAndAfter, FunSpec}
import sgit.createRepo.InitCommand
import sgit.io.{ReadFile, SearchingTools}
import sgit.localChangeRepo.{AddCommand, CommitCommand}
import sgit.objects.Commit

class LogCommandTest extends FunSpec with BeforeAndAfter {
  before {
    InitCommand.createTreeView()
    val repo :File = ".sgit/".toFile.parent
    val _: File=  (repo + "/" + "READMEBIS.md")
      .toFile
      .createIfNotExists()
    val _: File=  (repo + "/" + "READMES.md")
      .toFile
      .createIfNotExists()
      .appendLine("* Test")
  }
 after {
   if("READMES.md".toFile.exists) ".sgit/".toFile.parent + "/" + "READMES.md".toFile.delete()
   if("READMEBIS.md".toFile.exists) ".sgit/".toFile.parent + "/" + "READMEBIS.md".toFile.delete()
   if(".sgit/".toFile.exists) ".sgit/".toFile.delete()
 }

  describe("If the user writes the command sgit log in the sgit repository."){
    it("should display all the created commits (from the most recent to the oldest)"){
      //on one branch
      AddCommand.addAccordingTypeArg(Seq("READMES.md"))
      val firstCommit: Option[String] = CommitCommand.commit("First commit")
      AddCommand.addAccordingTypeArg(Seq("READMEBIS.md"))
      val secondCommit: Option[String] = CommitCommand.commit("Second commit")
      //last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()

      val allCommit: Seq[String] = LogCommand.showCommitProperties(lastCommit, Seq())

      val olderCommit: Option[Commit] = ReadFile.readCommitProperties(firstCommit)
      val recentCommit: Option[Commit] = ReadFile.readCommitProperties(secondCommit)

      assert(allCommit.length == 2)
      assert(allCommit.head == recentCommit.get.sha)
      assert(allCommit.last == olderCommit.get.sha)
    }
    it("should display nothing if there is no commit."){
      val lastCommit = None
      val allCommit: Seq[String] = LogCommand.showCommitProperties(lastCommit, Seq())

      assert(allCommit.isEmpty)
    }
  }
}
