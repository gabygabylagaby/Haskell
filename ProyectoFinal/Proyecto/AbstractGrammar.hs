module AbstractGrammar where

type Strings = String
type Paragraph = String
type H1 = String
type H2 = String
type H3 = String
type H4 = String
type H5 = String
type H6 = String
type Bold = String
type Italic = String
type Url = String
type Link = String
type Code = String
type Photo = String
type BlockText = String
type BlockTextDouble = String
type NumberList = String
type LinkText = String
type List = String

data Slides = Slides [Slide]
    deriving (Show)

data Slide = Slide TitleSlide BodySlide
    deriving (Show)

data TitleSlide = TitleSlide Strings
    deriving (Show)

data BodySlide = BodySlide [MarkdownText]
    deriving (Show)

data BodyLink = BodyLink Strings
    deriving (Show)

data MarkdownText = MdParagraph Paragraph
                   | MdH1 H1
                   | MdH2 H2
                   | MdH3 H3
                   | MdH4 H4
                   | MdH5 H5
                   | MdH6 H6
                   | MdList List 
                   | MdBold Bold 
                   | MdItalic Italic
                   | MdLink AutoBlock String
                   | MdCode Code
                    deriving (Show)

data AutoBlock = URL 
        deriving Show
