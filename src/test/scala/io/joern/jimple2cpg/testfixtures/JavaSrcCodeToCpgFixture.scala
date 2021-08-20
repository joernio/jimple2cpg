package io.joern.jimple2cpg.testfixtures

import io.joern.jimple2cpg.Jimple2Cpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.testfixtures.{CodeToCpgFixture, LanguageFrontend}

import java.io.File

class JavaSrcFrontend extends LanguageFrontend {

  override val fileSuffix: String = ".java"

  override def execute(sourceCodeFile: File): Cpg = {
    new Jimple2Cpg().createCpg(sourceCodeFile.getAbsolutePath)
  }
}

class JavaSrcCodeToCpgFixture extends CodeToCpgFixture(new JavaSrcFrontend) {}
