Feature: hyai indent
  In order to code Haskell faster
  As an Emacs user
  I want to indent code automatically

  Scenario: Beginning of buffer
    Given the buffer is empty
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

  Scenario: Before module
    Given the buffer is empty
    When I insert:
    """
    module Main where
    """
    And I place the cursor before "module"
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

    Given the buffer is empty
    When I insert:
    """
    
    module Main where
    """
    And I place the cursor before "module"
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

    Given the buffer is empty
    When I insert:
    """
    {-# LANGUAGE OverloadedStrings #-}
    module Main where
    """
    And I place the cursor before "module"
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

  Scenario: Before class
    Given the buffer is empty
    When I insert:
    """
    module Main where
    
    class Foo where
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

  Scenario: Before data
    Given the buffer is empty
    When I insert:
    """
    module Main where
    
    data Foo = Foo Int
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

  Scenario: Before import
    Given the buffer is empty
    When I insert:
    """
    import Predule
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

    Given the buffer is empty
    When I insert:
    """
    import Predule
    import Control.Monad
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

  Scenario: Before instance
    Given the buffer is empty
    When I insert:
    """
    module Main where
    
    data Foo = Foo Int
    
    instance Show Foo where
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

  Scenario: Before newtype
    Given the buffer is empty
    When I insert:
    """
    module Main where
    newtype Foo = Foo [String]
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

  Scenario: Before type
    Given the buffer is empty
    When I insert:
    """
    module Main
        (
          Foo
        ) where
    type Foo = String
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

  Scenario: After do
    Given the buffer is empty
    When I insert:
    """
    main = do
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    foo = bar
      where
        bar = do
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"

    Given the buffer is empty
    When I insert:
    """
    foobar = [ do
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(15)"

    Given the buffer is empty
    When I insert:
    """
    foobar = (do
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(14)"

    Given the buffer is empty
    When I insert:
    """
    foo = ( do
                print bar
                print baz
          , do
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(12)"

    Given the buffer is empty
    When I insert:
    """
    foo :: IO ()
    foo = case () of
        _    | True -> do
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(15)"

    Given the buffer is empty
    When I insert:
    """
    foobar = let baz = do
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(17)"

  Scenario: After where
    Given the buffer is empty
    When I insert:
    """
    module Main where
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

    Given the buffer is empty
    When I insert:
    """
    module Foo
        ( foo
        , bar
        , baz
        ) where
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

    Given the buffer is empty
    When I insert:
    """
    class Foo a where
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    foo = bar
      where
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    foo = bar
      where
        bar = baz
          where
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"

  Scenario: Before where
    Given the buffer is empty
    When I insert:
    """
    foo = bar
    where
    """
    And I place the cursor before "where"
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(2)"

    Given the buffer is empty
    When I insert:
    """
    foo = do
        bar
    where
    """
    And I place the cursor before "where"
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(2)"

    Given the buffer is empty
    When I insert:
    """
    foo = bar
      where
        bar = baz
    where
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(6)"

    Given the buffer is empty
    When I insert:
    """
    module Main where
    foo = bar
    where
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(2)"

    Given the buffer is empty
    When I insert:
    """
    instance Show Foo where
        show = showFoo
    where
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(6)"

  Scenario: After case ... of
    Given the buffer is empty
    When I insert:
    """
    foobar = case baz of
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 13)"

    Given the buffer is empty
    When I insert:
    """
    foobar = case baz of
    [
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 13)"

    Given the buffer is empty
    When I insert:
    """
    foobar = do
        case baz of
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"

    Given the buffer is empty
    When I insert:
    """
    foobar = do
        (case baz of
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(9)"

    Given the buffer is empty
    When I insert:
    """
    foobar = do
        let baz = case qux of
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(12 18)"

  Scenario: After then
    Given the buffer is empty
    When I insert:
    """
    foobar = do
        if foo
            then
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(12)"

  Scenario: Before then
    Given the buffer is empty
    When I insert:
    """
    foobar = do
        if foo
    then
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"

  Scenario: After else
    Given the buffer is empty
    When I insert:
    """
    foobar = if foo
                 then
                     bar
                 else
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(17)"

  Scenario: Before else
    Given the buffer is empty
    When I insert:
    """
    foobar = do
        if foo
            then
    else
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"

    Given the buffer is empty
    When I insert:
    """
    foobar = do
        if foo then
            bar
    else
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

  Scenario: Before in
    Given the buffer is empty
    When I insert:
    """
    foobar = let bar = 1
    in
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(9)"

  Scenario: After (
    Given the buffer is empty
    When I insert:
    """
    module Foo
        (
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(6)"

  Scenario: Before (
    Given the buffer is empty
    When I insert:
    """
    module Foo
    (
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    import Data.ByteString
    (
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    foobar = bazqux
    (
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(9 0)"

    Given the buffer is empty
    When I insert:
    """
    import Control.Applicative (
    (<$>)
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(29)"

    Given the buffer is empty
    When I insert:
    """
    import Data.Monoid ( mempty,
    (<>)
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(21)"

  Scenario: Before )
    Given the buffer is empty
    When I insert:
    """
    module Foo
        (
          foo
        , bar
        , baz
    )
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    module Foo
        (
          foo
        , (<^^>)
        , bar
    )
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

  Scenario: After {
    Given the buffer is empty
    When I insert:
    """
    data Person = Person {
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    data Person = Person
        {
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(6)"

  Scenario: Before {
    Given the buffer is empty
    When I insert:
    """
    data Person = Person
    {
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

  Scenario: Before }
    Given the buffer is empty
    When I insert:
    """
    data Person = Person {
    }
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(21)"

    Given the buffer is empty
    When I insert:
    """
    data Person = Person
        { firstName :: !String  -- ^ First name
        , lastName  :: !String  -- ^ Last name
        , age       :: !Int     -- ^ Age
    } deriving (Eq, Show)
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    main = do
        run defaultConfig {
            path = "test.txt"
    }
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

  Scenario: After [
    Given the buffer is empty
    When I insert:
    """
    foobarbaz = [
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    foo = do
        bar <- baz [
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"

  Scenario: Before [
    Given the buffer is empty
    When I insert:
    """
    foobar = concat
    [
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 9)"

    Given the buffer is empty
    When I insert:
    """
    instance Show Foo where
        show (Foo n) = concat
    [
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8 19)"

    Given the buffer is empty
    When I insert:
    """
    foobar = do
        barbaz
    [
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 8)"

  Scenario: Before ]
    Given the buffer is empty
    When I insert:
    """
    foobarbaz = [
    ]
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(12)"

    Given the buffer is empty
    When I insert:
    """
    foobarbaz = [
        qux
    ]
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    foobar = [
        foo,
        bar
    ]
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    foobar = [ foo
             , bar
    ]
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(9)"

    Given the buffer is empty
    When I insert:
    """
    foobar = [ foo,
               bar
    ]
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(9)"

    Given the buffer is empty
    When I insert:
    """
    foobar = [
        foobar
            baz,
        foobar
            qux
    ]
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

  Scenario: After ,
    Given the buffer is empty
    When I insert:
    """
    import Data.Text (foo,
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(18)"

    Given the buffer is empty
    When I insert:
    """
    foobar = concat [ baz,
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(18)"

    Given the buffer is empty
    When I insert:
    """
    foobar = concat [ baz,
                      qux,
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(18)"

    Given the buffer is empty
    When I insert:
    """
    foobar = concat [
        baz,
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    foobar = concat [
        baz,
        qux,
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    foobar = concat [
        putStrLn
           "baz",
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

  Scenario: Before ,
    Given the buffer is empty
    When I insert:
    """
    module Foo
        (
          foo
    ,
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    import Data.Text ( foo
                     , bar
    ,
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(17)"

    Given the buffer is empty
    When I insert:
    """
    import System.Process ( CreateProcess(..)
                          , StdStream(..)
    ,
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(22)"

    Given the buffer is empty
    When I insert:
    """
    foo = mconcat [ "This is ("
    ,
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(14)"

    Given the buffer is empty
    When I insert:
    """
    foo = bar {
        baz = 1,
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

  Scenario: Before ->
    Given the buffer is empty
    When I insert:
    """
    foobar :: ByteString
    ->
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(7)"

    Given the buffer is empty
    When I insert:
    """
    foobar :: ByteString
           -> (Char -> Char)
    ->
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(7)"

    Given the buffer is empty
    When I insert:
    """
    foo xs = case xs of
        (x:xs') | x == 'a'
    ->
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(14)"

  Scenario: Before =>
    Given the buffer is empty
    When I insert:
    """
    foobar :: (Monad m)
    =>
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(7)"

  Scenario: After -> line
    Given the buffer is empty
    When I insert:
    """
    foo args =
        case args of
            [] -> return ()
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8 14 0 4)"

  Scenario: After =
    Given the buffer is empty
    When I insert:
    """
    foobar =
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    main = do
        let foobar =
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(12)"

    Given the buffer is empty
    When I insert:
    """
    main = let foobar =
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(15)"

  Scenario: Before =
    Given the buffer is empty
    When I insert:
    """
    data Foo
    = Bar String
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

  Scenario: After = line
    Given the buffer is empty
    When I insert:
    """
    main = foobar
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(7 0)"

    Given the buffer is empty
    When I insert:
    """
    
    f = foobar
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 0)"

    Given the buffer is empty
    When I insert:
    """
    person = Person
        { firstName = "John"

    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(18)"

    Given the buffer is empty
    When I insert:
    """
    person = Person
        { firstName = "John"
        , lastName = "Smith"

    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(17)"

  Scenario: After let line
    Given the buffer is empty
    When I insert:
    """
    foobar = let bar = True
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(13 19)"

    Given the buffer is empty
    When I insert:
    """
    main = do
        let foo = 1
            bar = 2
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8 14 0 4)"

    Given the buffer is empty
    When I insert:
    """
    main = foo
      where
        bar = do
            baz
            let qux = 1
                quux = 2
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(12 19 0 4 8)"

  Scenario: Before |
    Given the buffer is empty
    When I insert:
    """
    data Foo = Bar
    |
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(9)"

    Given the buffer is empty
    When I insert:
    """
    data Foo = Bar
             | Baz
    |
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(9)"

    Given the buffer is empty
    When I insert:
    """
    filter p (x:xs)
    |
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    filter p (x:xs)
        | p x       = x : filter p xs
    |
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4)"

    Given the buffer is empty
    When I insert:
    """
    filter p (x:xs) | p x       = x : filter p xs
    |
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(16)"

  Scenario: Before | in where context
    Given the buffer is empty
    When I insert:
    """
    main = do
        foo
      where
        foo
    |
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"

  Scenario: Before | in case context
    Given the buffer is empty
    When I insert:
    """
    foobar x = case x of
        Just y
    |
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"

    Given the buffer is empty
    When I insert:
    """
    foobar x = case x of
                   Just y
    |
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(19)"

  Scenario: After import
    Given the buffer is empty
    When I insert:
    """
    import System.IO

    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0 4)"

    Given the buffer is empty
    When I insert:
    """
    import System.IO (getChar)

    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

    Given the buffer is empty
    When I insert:
    """
    import qualified System.IO as IO
        ( getChar
        , putChar
        )
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0)"

  Scenario: After then line
    Given the buffer is empty
    When I insert:
    """
    foobar = do
        if foo
            then bar
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(13 17)"

  Scenario: After else line
    Given the buffer is empty
    When I insert:
    """
    foobar = do
        if foo
            then bar
            else baz
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(13 17 0 4)"

    Given the buffer is empty
    When I insert:
    """
    foobar = do
        if foo then bar else baz
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(25 29 0 4)"


  Scenario: After if then line
    Given the buffer is empty
    When I insert:
    """
    foobar = do
        if foo then
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(8)"

    Given the buffer is empty
    When I insert:
    """
    foobar = if foo then
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(13)"

  Scenario: After <- line
    Given the buffer is empty
    When I insert:
    """
    main = do
        args <- getArgs
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 12 0)"

  Scenario: After ( line
    Given the buffer is empty
    When I insert:
    """
    main = do
        putStrLn (foo
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(14 18)"

    Given the buffer is empty
    When I insert:
    """
    main = do
        putStrLn ( foo
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(15 19)"

    Given the buffer is empty
    When I insert:
    """
    main = do
        foobar (Baz "qux"
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(12 16)"

  Scenario: After [ line
    Given the buffer is empty
    When I insert:
    """
    foobar =
        [ buzqux
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(6 10)"

  Scenario: After { line
    Given the buffer is empty
    When I insert:
    """
    data Person = Person
        { firstName -- ^ First name
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(6 10)"

  Scenario: After , line
    Given the buffer is empty
    When I insert:
    """
    main = putStrLn $ concat
        [ foo
        , bar
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(6 10)"

  Scenario: After normal line
    Given the buffer is empty
    When I insert:
    """
    main = do
        foobar
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 8 0)"

    Given the buffer is empty
    When I insert:
    """
    import Data.Text
    
    main = do
        foobar ()
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 8 0)"

    Given the buffer is empty
    When I insert:
    """
    foo a' = do
        bar a' 'a'
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 8 0)"

    Given the buffer is empty
    When I insert:
    """
    foo x y = case x of
        True | y == 'a'
               -> putStrLn "a"
             | y == 'b'
               -> putStrLn "b"
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(14 0 4)"

    Given the buffer is empty
    When I insert:
    """
    main = do
        let foobar = 1
            barbaz =
                "qux"
    
    """
    And I call hyai-indent-candidates at the current point
    # 0 is not proper indentation, but left as is for now.
    Then indent candidates are "(12 16 0 4 8)"

    Given the buffer is empty
    When I insert:
    """
    main = do
        foobar
      where
        foobar = bazqux
          where
            baz = do
                qux
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(12 16 0 4 8)"

  Scenario: In list comprehension
    Given the buffer is empty
    When I insert:
    """
    foo = let xy = [ (x, y) | x <- [1, 2, 3],
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(26)"

  Scenario: Hanging lambda
    Given the buffer is empty
    When I insert:
    """
    foo = alloca 10 $ \a ->
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 6)"

    Given the buffer is empty
    When I insert:
    """
    foobar = alloca 10 $ \a ->
        alloca 20 $ \b ->
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(4 8)"

  Scenario: After type signature
    Given the buffer is empty
    When I insert:
    """
    main :: IO ()
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0 8)"

    Given the buffer is empty
    When I insert:
    """
    sort :: [a] -> [a]
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0 8 15)"

    Given the buffer is empty
    When I insert:
    """
    foobar :: (Monad m) => a -> m a
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0 10 28)"

    Given the buffer is empty
    When I insert:
    """
    foobar :: Text
           -> (Char -> Char)
           -> Text
    
    """
    And I call hyai-indent-candidates at the current point
    Then indent candidates are "(0 10)"
