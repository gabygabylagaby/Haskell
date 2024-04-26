import AbstractGrammar
import Parser
import Scanner(scanner)
import UU.Parsing

-- Convertir una Slide a HTML
slideToHtml :: Slide -> String
slideToHtml (Slide (TitleSlide title) (BodySlide blocks)) =
    "<div class=\"slide\">\n" ++
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
markdownBlockToHtml (MdLink URL link) = "<a href=\"" ++ link ++ "\">" ++ link ++ "</a><br> "
markdownBlockToHtml (MdCode text) = "<pre><code class=\"language-haskell\">" ++ text ++ "</code></pre>\n"
markdownBlockToHtml (MdImage text) = "    <img src=\"" ++ text ++ "\" alt=\"\">\n"
markdownBlockToHtml (MdListAsterisc items) = "<ol>\n" ++ concatMap listItemToHtml items ++ "</ol>\n"
  where
    listItemToHtml item = "<li>" ++ extractListAsteriscContent item ++ "</li>\n"

extractListAsteriscContent :: ListAsterisc -> String
extractListAsteriscContent (ListNumeral content) = content


-- Convertir una lista de Slides a HTML
slidesToHtml :: Slides -> String
slidesToHtml (Slides slides) = "<!DOCTYPE html>\n<html>\n<head>\n<title>Presentación</title>\n" ++
                               "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/style.css\">\n" ++
                               "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.25.0/themes/prism.min.css\">\n" ++
                               "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.0/highlight.min.js\"></script>\n" ++
                               "<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.5.0/styles/darcula.min.css\">\n" ++
                               "</head>\n<body class=\"presentation\">\n" ++  -- Agrega la clase "presentation"
                               concatMap slideToHtml slides ++
                               "<div class=\"prev\" onclick=\"plusSlides(-1)\">❮ Anterior</div>\n" ++
                               "<div class=\"next\" onclick=\"plusSlides(1)\">Siguiente ❯</div>\n" ++
                               "<script>hljs.highlightAll();</script>\n" ++
                               "<script src=\"javascript/slides.js\"></script>\n" ++ 
                               "</body>\n</html>"



-- Ejemplo de uso
main :: IO ()
main = do input <- readFile "slide.p5"
          let token = scanner input
          putStrLn(show token) 
          tree <- parseIO pSlides token
          putStrLn(show tree) 
          writeFile "presentation.html" (slidesToHtml tree)
