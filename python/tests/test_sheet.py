import unittest as UT
import sheet as CS

import os

class TestSheet(UT.TestCase):
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    # csv
    def test_csv_01(self):
        self.assertEqual( CS.csv2List("./tests/sample/empty.csv")
                        , [ ]
                        )
    def test_csv_02(self):
        self.assertEqual( CS.csv2List("./tests/sample/list1.csv")
                        , [ ["001", "aaa"]
                          , ["002", "bbb"]
                          , []
                          ]
                        )
    def test_csv_03(self):
        self.assertEqual( CS.csv2List("./tests/sample/list2.csv")
                        , [ ["001", "aaa","xxx"]
                          , ["002", "bbb",""]
                          , ["003", ""   ,"yyy"]
                          ]
                        )
    def test_csv_04(self):
        self.assertEqual( CS.csv2List_removeEmpty("./tests/sample/dirty01.csv")
                        , [ ["001", "aaa","xxx"]
                          , ["002", "bbb",""]
                          , ["003", ""   ,"yyy"]
                          ]
                        )

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    # directory
    def test_dir_01(self):
        tests_dir = os.path.join(CS.getCurrDirPath(), "tests")
        sample_dir = os.path.join(tests_dir, "sample")
        tgt_dir = os.path.join(sample_dir, "for_dir")
        self.assertEqual( CS.getContetsNames(tgt_dir)
                        , ( [ "dir1"
                            , "dir2"
                            ]
                          , [ "md1.md"
                            , "text1.txt"
                            , "text2.txt"
                            ]
                          )
                        )

    def test_dir_02(self):
        tests_dir = os.path.join(CS.getCurrDirPath(), "tests")
        sample_dir = os.path.join(tests_dir, "sample")
        tgt_dir = os.path.join(sample_dir, "for_dir")
        self.assertEqual( CS.findFileNames(tgt_dir)
                        , [ "md1.md"
                          , "text1.txt"
                          , "text2.txt"
                          ]
                        )

    def test_dir_03(self):
        tests_dir = os.path.join(CS.getCurrDirPath(), "tests")
        sample_dir = os.path.join(tests_dir, "sample")
        tgt_dir = os.path.join(sample_dir, "for_dir")
        self.assertEqual( CS.findFileNames(tgt_dir, "md")
                        , [ "md1.md"
                          ]
                        )




# main
if(__name__ == "__main__"):
    UT.main()

