import Data.Persist

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
    ms <- createMemoryStore
    showMemoryStore ms >>= putStrLn
    initTable ms personTable
    showMemoryStore ms >>= putStrLn
    rid <- createRecord ms personTable michael24
    print rid
    showMemoryStore ms >>= putStrLn

    putStrLn "\nfilterTable"
    filterTable ms personTable [Filter "age" (FVInt 24) [EQ, GT]] print' ()
    filterTable ms personTable [Filter "age" (FVInt 25) [EQ, GT]] print' ()

    readRecord ms personTable rid >>= print
    updateRecord ms personTable rid michael25
    showMemoryStore ms >>= putStrLn
    readRecord ms personTable rid >>= print

    putStrLn "\nfilterTable, 25 in db"
    filterTable ms personTable [Filter "age" (FVInt 24) [EQ, GT]] print' ()
    filterTable ms personTable [Filter "age" (FVInt 25) [EQ, GT]] print' ()

    deleteRecord ms personTable rid
    showMemoryStore ms >>= putStrLn
    readRecord ms personTable rid >>= print

    putStrLn "\nfilterTable, empty db"
    filterTable ms personTable [Filter "age" (FVInt 24) [EQ, GT]] print' ()
    filterTable ms personTable [Filter "age" (FVInt 25) [EQ, GT]] print' ()
