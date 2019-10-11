package sgit.localChangeRepo

import org.scalatest.{BeforeAndAfter, FunSpec}
import sgit.createRepo.InitCommand
import better.files._
import sgit.io.{ReadFile, RepoSearching, SearchingTools}
import sgit.objects.StagedLine

class StatusCommandTest extends FunSpec with BeforeAndAfter{
  before{
    InitCommand.createTreeView()
    val folder: File = ("test/")
      .toFile
      .createIfNotExists(true,false)
    val _: File=  ("test/READMES.md")
      .toFile
      .createIfNotExists(false, true)
    val _: File=  ("test/READMEBIS.md")
      .toFile
      .createIfNotExists(false, true)
  }
  after{
    if(".sgit/".toFile.exists) ".sgit".toFile.delete()
    if("test/".toFile.exists) "test".toFile.delete()
  }
  describe("If the user write the command sgit status in the sgit repository."){
    it("should classify all the files in untracked files when there is no commit add add command create."){
      val repo:Seq[File] = RepoSearching.searchAllDirectoryFile("READMES.md")
      val stagedContent :Option[Seq[StagedLine]] = ReadFile.readStaged()
      if(stagedContent.isDefined) {
        val untracked: Option[Seq[String]] = SearchingTools.searchedUntrackedFiles(repo,stagedContent)

        assert(untracked.get.length == 2)
      }
    }
    it("should classify the files in changes to be committed added if the user add the files."){
      val repo:Seq[File] = RepoSearching.searchAllDirectoryFile("READMES.md")
      AddCommand.addAccordingTypeArg(Seq("READMES.md","READMEBIS.md"))

      //retrieve last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()
      //if the last commit exists
      if (lastCommit.isDefined) {
        //content of last commit
        val commitContent = ReadFile.readCommit(lastCommit.get)
        val modifiedFiles: Option[Seq[StagedLine]] = SearchingTools.searchedModifiedFiles(repo, commitContent)

        assert(modifiedFiles.get.length == 2)
      }
    }
    /*it("should classify the files in not staged for commit if the file has benn modified after the commit."){
      val repo:Seq[File] = RepoSearching.searchAllDirectoryFile("READMES.md")
      AddCommand.addAccordingTypeArg(Seq("READMES.md","READMEBIS.md"))
      CommitCommand.commit("My first commit")
      //modify a file
      val _: File=  ("test/READMES.md")
        .toFile
        .appendLine("test")

      AddCommand.addAccordingTypeArg(Seq("READMES.md"))

      //retrieve last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()
      //if the last commit exists
      if (lastCommit.isDefined) {
        //content of last commit
        val commitContent = ReadFile.readCommit(lastCommit.get)
        val modifiedFiles: Option[Seq[StagedLine]] = SearchingTools.searchedModifiedFiles(repo, commitContent)

        assert(modifiedFiles.get.length == 1)
      }

    }*/
  }
}
