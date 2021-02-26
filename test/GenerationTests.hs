  module GenerationTests where

import MGeneration
import Test.HUnit

testMBSize1 = TestCase (assertEqual "for: makeBoard (10, 8)"
                                     (10, 8)
                                     ((length mb108), (length (mb108 !! 0))))
testMBSize2 = TestCase (assertEqual "for: makeBoard (18, 14)"
                                    (18, 14)
                                    ((length mb108), (length (mb1814 !! 0))))
testMBSize3 = TestCase (assertEqual "for: makeBoard (24, 20)"
                                    (24, 20)
                                    ((length mb108), (length (mb2420 !! 0))))
