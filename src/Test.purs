module Test where
import Main
import Prelude
import Data.String as Str
import Data.Array as Ary
import Type.RowList
import Type.Proxy

type Student = { studentId :: Int, name :: String, age :: Int }
type School  = { name :: String
               , age :: Int
               , students :: Array Student
               }




studentValidator = { name: [ notEmpty ], age: [ atLeast 5] }

schoolValidator = { name: [ Str.length >>> atLeast 5]
                  , age: [ atLeast 6 ]
                  , asdf: []
                  , students: [ lengthAtLeast 5
                              , each studentValidator ]
                  }

studentA = { name: "", age: 1 }
studentB = { name: "bob", age: 10}
studentC = { name: "charles", age: 2 }

mySchool = { name: "hi"
           , age: 100
           , students: [ studentA, studentB, studentC ]
           }

expectedResult =
  { name: [FieldError "too short"]
  , age: []
  , students: [ FieldError "too short" ]
  }

asdf = allErrorsEmpty proxy {name: [3], age: []}
  where proxy :: Proxy (Cons "age" (Array Int) (Cons "name" (Array Int) Nil))
        proxy = Proxy
