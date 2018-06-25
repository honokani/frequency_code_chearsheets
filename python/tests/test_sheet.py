import unittest as UT
import sheet as CS

class TestSheet(UT.TestCase):
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
                          , ["003", "ccc","yyy"]
                          ]
                        )
    def test_csv_04(self):
        self.assertEqual( CS.csv2List_removeEmpty("./tests/sample/dirty01.csv")
                        , [ ["001", "aaa","xxx"]
                          , ["002", "bbb",""]
                          , ["003", "ccc","yyy"]
                          ]
                        )






if(__name__ == "__main__"):
    UT.main()

