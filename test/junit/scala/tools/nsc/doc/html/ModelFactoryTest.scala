package scala.tools.nsc.doc.html

import org.junit.Assert.assertEquals
import org.junit.Test

import scala.tools.nsc.doc.model.ModelFactory._

class ModelFactoryTest {
  def t(expected: String, template: String,
        filePath: String = null, fileExt: String = null, line: Int = 0, tplOwner: String = null, tplName: String = null) =
    assertEquals(expected, expandUrl(template, filePath, fileExt, filePath + fileExt, line, tplOwner, tplName))

  @Test def sourceUrlReplace(): Unit = {
    // relative path: insert `/` if path follows a word character
    t("/base/p/file.scala", "/base€{FILE_PATH_EXT}", filePath = "p/file", fileExt = ".scala")
    t("/base/p/file.scala", "/base/€{FILE_PATH_EXT}", filePath = "p/file", fileExt = ".scala")
    t("/base.p/file.scala", "/base.€{FILE_PATH_EXT}", filePath = "p/file", fileExt = ".scala")
    t("p/file.scala", "€{FILE_PATH_EXT}", filePath = "p/file", fileExt = ".scala")

    t("/base/p/file.scala", "/base€{FILE_PATH_EXT}", filePath = "/p/file", fileExt = ".scala")
    t("/base//p/file.scala", "/base/€{FILE_PATH_EXT}", filePath = "/p/file", fileExt = ".scala")

    // file extension: don't duplicate `.`
    t("/base/p/file.scala", "/base€{FILE_PATH}€{FILE_EXT}", filePath = "p/file", fileExt = ".scala")
    t("/base/p/file.scala", "/base/€{FILE_PATH}.€{FILE_EXT}", filePath = "p/file", fileExt = ".scala")
    t("p/file.scala", "€{FILE_PATH}.€{FILE_EXT}", filePath = "p/file", fileExt = ".scala")
    t("p/file", "€{FILE_PATH}", filePath = "p/file", fileExt = ".scala")
    t(".scala", "€{FILE_EXT}", filePath = "", fileExt = ".scala")
  }
}
