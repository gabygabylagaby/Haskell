import AbstractGrammar
import Parser
import Scanner(scanner)
import UU.Parsing

-- Convertir una Slide a HTML
slideToHtml :: Slide -> String
slideToHtml (Slide (TitleSlide title) (BodySlide blocks)) =
    "<div class=\"slide\" style=\"text-align: center;\">\n" ++
    "<h1>" ++ title ++ "</h1>\n" ++
    "<div class=\"content\">\n" ++
    concatMap markdownBlockToHtml blocks ++
    "</div>\n" ++
    "</div>\n"

-- Convertir un MarkdownBlock a HTML
markdownBlockToHtml :: MarkdownText -> String
markdownBlockToHtml (MdParagraph text) = "<p>" ++ text ++ "</p>\n"
markdownBlockToHtml (MdH1 text) = "<h1>" ++ text ++ "</h1>\n"
markdownBlockToHtml (MdH2 text) = "<h2>" ++ text ++ "</h2>\n"
markdownBlockToHtml (MdH3 text) = "<h3>" ++ text ++ "</h3>\n"
markdownBlockToHtml (MdH4 text) = "<h4>" ++ text ++ "</h4>\n"
markdownBlockToHtml (MdH5 text) = "<h5>" ++ text ++ "</h5>\n"
markdownBlockToHtml (MdH6 text) = "<h6>" ++ text ++ "</h6>\n"
markdownBlockToHtml (MdList text) = "<li>" ++ text ++ "</li>\n"
markdownBlockToHtml (MdBold text) = "<strong>" ++ text ++ "</strong>"
markdownBlockToHtml (MdItalic text) = "<em>" ++ text ++ "</em>" 
markdownBlockToHtml (MdLink URL link) = "<a href=\"" ++ link ++ "\">" ++ link ++ "</a>\n"

-- Convertir una lista de Slides a HTML
slidesToHtml :: Slides -> String
slidesToHtml (Slides slides) = "<!DOCTYPE html>\n<html>\n<head>\n<title>Presentación</title>\n</head>\n<body>\n" ++
                               concatMap slideToHtml slides ++
                               "</body>\n</html>"

-- Ejemplo de uso
main :: IO ()
main = do input <- readFile "slide.p5"
          let token = scanner input
          putStrLn(show token) 
          tree <- parseIO pSlides token
          putStrLn(show tree) 
          writeFile "presentation.html" (slidesToHtml tree)
