{-# Language TemplateHaskell, QuasiQuotes, FlexibleContexts, DeriveDataTypeable, Haskell2010, OverloadedStrings #-}

module KotlinTemplates
     where

import Data.Text (Text, pack)
import Data.Text as DT (concat, lines)
import Data.Maybe (fromMaybe)
import Data.Data
import Data.Typeable
import Parser
import Data.List
import Text.Shakespeare.Text
import Control.Monad.Supply


{-
 - This file is full of templates of Kotlin code.
 - The templates are written in 'shakespeare' format; details can be found
 - here: https://www.yesodweb.com/book/shakespearean-templates#shakespearean-templates_other_shakespeare
 -
 - Some understanding of the Diff datatype defined in src/Parser.hs is needed to write useful tests.
 -}

kotlinPre :: Text -> Text
kotlinPre class_name = [st|package org.fejoa.fs

import junit.framework.TestCase
import kotlinx.coroutines.experimental.newSingleThreadContext
import kotlinx.coroutines.experimental.runBlocking
import org.fejoa.AccountIO
import org.fejoa.FejoaContext
import org.fejoa.UserData
import org.fejoa.crypto.CryptoSettings
import org.fejoa.crypto.generateSecretKeyData
import org.fejoa.fs.fusejnr.FejoaFuse
import org.fejoa.fs.fusejnr.Utils
import org.fejoa.storage.StorageDir
import org.fejoa.support.StorageLib
import org.fejoa.support.StreamHelper
import org.fejoa.support.toInStream
import org.junit.After
import org.junit.Before
import org.junit.Test

import java.io.*
import java.util.ArrayList


class #{class_name} {
    private val cleanUpDirs: MutableList<String> = ArrayList()
    private val baseDir = "fejoafs"
    private var userData: UserData? = null

    @Before
    fun setUp() = runBlocking {
        cleanUpDirs.add(baseDir)

        val context = FejoaContext(AccountIO.Type.CLIENT, baseDir, "userData",
                newSingleThreadContext("fuse test context"))
        userData = UserData.create(context)
    }

    @After
    fun tearDown() {
        for (dir in cleanUpDirs)
            StorageLib.recursiveDeleteFile(File(dir))
    }

    private suspend fun createStorageDir(userData: UserData, storageContext: String): StorageDir {
        val keyData = CryptoSettings.default.symmetric.generateSecretKeyData()
        val branch = userData.createStorageDir(storageContext, UserData.generateBranchName().toHex(),
                "fuse test branch", keyData)
        val storageDir = userData.getStorageDir(branch)
        userData.commit()
        return storageDir
    }

    @Throws(IOException::class)
    private fun assertContent(file: File, content: String) {
        val read = String(StreamHelper.readAll(FileInputStream(file).toInStream()))
        TestCase.assertEquals(content, read)
    }

    @Test
    fun testBasics() = runBlocking {
        val storageDir = createStorageDir(userData!!, "testContext")
|]

kotlinPost = [st|    }
}
|]

kotlinSection

kotlinDiffApplication :: Text -> ChangeOp -> Text
kotlinDiffApplication filename (ChangeOp (Pos startline startextent endline endextent) l) = [st|
    val mountedTestFile = File(mountPoint, #{filename})
    val user
|]
