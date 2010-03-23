import Data.Persist
import Data.Persist.SQLite

data Person = Person
    { name :: String
    , age :: Int
    }
    deriving Show

personTable = Table
    { tableName = "person"
    , tableFields = [nameField, ageField]
    , tableFreeze = freezePerson
    , tableThaw = thawPerson
    }

nameField = Field "name" FTString False
ageField = Field "age" FTInt False
freezePerson (Person n a) = [("name", FVString n), ("age", FVInt a)]
thawPerson [("name", FVString n), ("age", FVInt a)] = Just $ Person n a
thawPerson _ = Nothing

michael24 = Person "Michael" 24
michael25 = Person "Michael" 25

print' x () = print x >> return (Right ())

main = do
    db <- loadSQLite ":memory:"
    --db <- loadSQLite "test.db3"
    initTable db personTable
    rid <- createRecord db personTable michael24
    print rid

    putStrLn "\nfilterTable"
    filterTable db personTable [Filter "age" (FVInt 24) [EQ, GT]] print' ()
    filterTable db personTable [Filter "age" (FVInt 25) [EQ, GT]] print' ()

    readRecord db personTable rid >>= print
    updateRecord db personTable rid michael25
    readRecord db personTable rid >>= print

    putStrLn "\nfilterTable, 25 in db"
    filterTable db personTable [Filter "age" (FVInt 24) [EQ, GT]] print' ()
    filterTable db personTable [Filter "age" (FVInt 25) [EQ, GT]] print' ()

    deleteRecord db personTable rid
    readRecord db personTable rid >>= print

    putStrLn "\nfilterTable, empty db"
    filterTable db personTable [Filter "age" (FVInt 24) [EQ, GT]] print' ()
    filterTable db personTable [Filter "age" (FVInt 25) [EQ, GT]] print' ()
