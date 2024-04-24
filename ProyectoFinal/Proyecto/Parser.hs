module Parser where
 
import UU.Parsing
import Scanner
import AbstractGrammar
import UU.Scanner (pKey)
 
pSlides = Slides <$> pList pSlide
 
pSlide = Slide <$> pTitleSlide <*> pBodySlide <* pEndSlide "---"
 
pTitleSlide = TitleSlide <$ pKeyword "!" <*> pStrings
 
pBodySlide = BodySlide <$ pOpenBlock "{" <*> pList pMarckdownText <* pEndBlock "}" 
 
pMarckdownText = MdParagraph <$> pStrings     
               <|> MdH1 <$ pKeyword "#" <*> pStrings
               <|> MdH2 <$ pKeyword "##" <*> pStrings
               <|> MdH3 <$ pKeyword "###" <*> pStrings
               <|> MdH4 <$ pKeyword "####" <*> pStrings
               <|> MdH5 <$ pKeyword "#####" <*> pStrings
               <|> MdH6 <$ pKeyword "######" <*> pStrings
               <|> MdList <$ pKeyword "+" <*> pStrings
               <|> MdBold <$ pKeyword "*" <*> pStrings <* pKeyword "*"
               <|> MdItalic <$ pKeyword "_" <*> pStrings <* pKeyword "_"
               <|> pLinkBlock
               <|> MdCode <$ pKeyword "```" <*> pStrings <* pKeyword "```"
            
pLinkBlock :: Parser Token MarkdownText
pLinkBlock = pURL

pURL :: Parser Token MarkdownText
pURL = MdLink URL <$ pKeyword "<" <*> pStrings <* pKeyword ">"
     

{- 
<|> -> "alternativa" o la "opción" entre diferentes combinadores de parser
<*> ->  se utiliza para combinar dos acciones

 -}
--- Join Scanner with Parser
instance Symbol Token
 
getValue:: Token -> String
getValue (Token _ v _ _) = v
 
tSym :: Type -> String -> Parser Token String
tSym typ value = getValue <$> pSym (Token typ value 0 0)
 
pKeyword :: String -> Parser Token String
pKeyword = tSym Keyword
 
pOpenBlock :: String -> Parser Token String
pOpenBlock = tSym OpenBlock
 
pEndBlock :: String -> Parser Token String
pEndBlock = tSym EndBlock
 
pEndSlide :: String -> Parser Token String
pEndSlide = tSym EndSlide
 
tStr = getValue <$> pSym (Token String "" 0 0)

pStrings :: Parser Token String
pStrings = tStr
