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
    it("should classify all the files in untracked files when there is no commit and add command create."){
      val repo:Seq[File] = RepoSearching.searchAllDirectoryFile("READMES.md")
      val stagedContent :Option[Seq[StagedLine]] = ReadFile.readStaged()
      if(stagedContent.isDefined) {
        val untracked: Option[Seq[String]] = SearchingTools.searchedUntrackedFiles(repo,stagedContent)

        assert(untracked.get.length == 2)
        assert(untracked.equals(Seq("READMES.md","READMEBIS.md")))
      }
    }
    it("should classify the files in \"changes to be committed Added\" if the user add the files when there is no commit."){
      val repo:Seq[File] = RepoSearching.searchAllDirectoryFile("READMES.md")
      AddCommand.addAccordingTypeArg(Seq("READMES.md"))

      //retrieve last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()
      //if the last commit exists
      if (lastCommit.isDefined) {
        //content of last commit
        val commitContent = ReadFile.readCommit(lastCommit.get)
        val modifiedFiles: Option[Seq[StagedLine]] = SearchingTools.searchedModifiedFiles(repo, commitContent)

        assert(modifiedFiles.get.length == 1)
        assert(modifiedFiles.get.map(f => f.path) == Seq("READMES.md"))
      }
    }
    it("should classify the files in \"to be committed Modified\" if the file has been modified and added after the commit."){
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
        //retrieve the modified files
        val modifiedFiles: Option[Seq[StagedLine]] = SearchingTools.searchedModifiedFiles(repo, commitContent)

        val stagedFile :Option[Seq[StagedLine]]= ReadFile.readStaged()
        val newFileCommitted= SearchingTools.toBeCommittedFileAdded(stagedFile.get,commitContent)

        assert(modifiedFiles.get.length == 1)
        assert(modifiedFiles.get.map(f => f.path) == Seq("READMES.md"))
        assert(newFileCommitted.isEmpty)
      }
    }
    it("should classify the files in \"not staged for commit Modified\" if the file has been modified after the commit."){
      val repo:Seq[File] = RepoSearching.searchAllDirectoryFile("READMES.md")
      AddCommand.addAccordingTypeArg(Seq("READMES.md","READMEBIS.md"))
      CommitCommand.commit("My first commit")
      //modify a file
      val _: File=  ("test/READMES.md")
        .toFile
        .appendLine("test")

      //retrieve last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()
      //if the last commit exists
      if (lastCommit.isDefined) {
        //content of last commit
        val commitContent = ReadFile.readCommit(lastCommit.get)
        //retrieve the modified files
        val modifiedFiles: Option[Seq[StagedLine]] = SearchingTools.searchedModifiedFiles(repo, commitContent)
        //retrieve staged file content
        val stagedFile: Option[Seq[StagedLine]] = ReadFile.readStaged()
        //retrieve the modified file which are not in the staged file (added)
        val fileModifiedWD: Seq[StagedLine] = modifiedFiles.get.diff(stagedFile.get)

        assert(modifiedFiles.get.length == 1)
        assert(modifiedFiles.get.map(f => f.path) == Seq("READMES.md"))
        assert(fileModifiedWD.head.path == "READMES.md")
      }
    }
    it("should classify the files in \"to be committed Added\" if the file has been created and added after a commit with another files."){
      val repo:Seq[File] = RepoSearching.searchAllDirectoryFile("READMES.md")
      AddCommand.addAccordingTypeArg(Seq("READMES.md","READMEBIS.md"))
      CommitCommand.commit("My first commit")
      //create and add a new file
      val _: File=  ("test/TEST.md").toFile.createFile()
      AddCommand.addAccordingTypeArg(Seq("TEST.md"))
      //last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()
      //if the last commit exists
      if (lastCommit.isDefined) {
        //content of last commit
        val commitContent: Seq[StagedLine] = ReadFile.readCommit(lastCommit.get)
        //retrieve the modified files
        val modifiedFiles: Option[Seq[StagedLine]] = SearchingTools.searchedModifiedFiles(repo, commitContent)

        assert(modifiedFiles.get.isEmpty)
      }
    }
    it("should classify the files in \"to be committed Added\" if the file has been created and added with other files after a commit with another files."){
      val repo:Seq[File] = RepoSearching.searchAllDirectoryFile("READMES.md")
      AddCommand.addAccordingTypeArg(Seq("READMES.md","READMEBIS.md"))
      CommitCommand.commit("My first commit")
      //create and add a new file
      val _: File=  ("test/TEST.md").toFile.createFile()
      //modify a file
      val _: File=  ("test/READMES.md").toFile.appendLine("test")
      AddCommand.addAccordingTypeArg(Seq("TEST.md","READMES.md"))

      //last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()
      //if the last commit exists
      if (lastCommit.isDefined) {
        //content of last commit
        val commitContent: Seq[StagedLine] = ReadFile.readCommit(lastCommit.get)
        //retrieve the modified files
        val modifiedFiles: Option[Seq[StagedLine]] = SearchingTools.searchedModifiedFiles(repo, commitContent)
        //retrieve staged file content
        val stagedFiles: Option[Seq[StagedLine]] = ReadFile.readStaged()
        //retrieve the new files added
        val newFileToBeCommitted: Option[Seq[StagedLine]] = SearchingTools.toBeCommittedFileAdded(stagedFiles.get, commitContent)

        assert(modifiedFiles.get.length==1)
        assert(modifiedFiles.get.map(f => f.path) == Seq("READMES.md"))
        assert(newFileToBeCommitted.get.length ==1)
        assert(newFileToBeCommitted.get.map(f => f.path) == Seq("TEST.md"))
      }
    }
  }
}
