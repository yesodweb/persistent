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

main = do
    ms <- createMemoryStore
    showMemoryStore ms >>= putStrLn
    initTable ms personTable
    showMemoryStore ms >>= putStrLn
    rid <- createRecord ms personTable michael24
    print rid
    showMemoryStore ms >>= putStrLn
    readRecord ms personTable rid >>= print
    updateRecord ms personTable rid michael25
    showMemoryStore ms >>= putStrLn
    readRecord ms personTable rid >>= print
    deleteRecord ms personTable rid
    showMemoryStore ms >>= putStrLn
    readRecord ms personTable rid >>= print
