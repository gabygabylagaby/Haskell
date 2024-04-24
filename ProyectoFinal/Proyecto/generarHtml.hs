import AbstractGrammar
import Parser
import Scanner(scanner)
import UU.Parsing

-- Convertir una Slide a HTML
slideToHtml :: Slide -> String
slideToHtml (Slide (TitleSlide title) (BodySlide blocks)) =
    "<div class=\"slide\" style=\"text-align: center; background-image: url('https://img.freepik.com/vector-gratis/fondo-textura-acuarela-detallada-simbolos-manchas_1048-17565.jpg?w=996&t=st=1713996219~exp=1713996819~hmac=2dd63c5077440a72165b5a3269dc85caecd6cadae9cc94ab63b5d5322cf601b8'); background-size: cover; background-position: center;\">\n" ++
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
markdownBlockToHtml (MdCode text) = "<code>" ++ text ++ "</code>\n"
markdownBlockToHtml (MdImage text) = "    <img src=\"" ++ text ++ "\" alt=\"\">\n"

-- Convertir una lista de Slides a HTML
slidesToHtml :: Slides -> String
slidesToHtml (Slides slides) = "<!DOCTYPE html>\n<html>\n<head>\n<title>Presentación</title>\n" ++
                               "<style>\n" ++
                               "body { font-family: Arial, sans-serif; margin: 0; padding: 0; }\n" ++
                               ".slide { display: none; position: absolute; top: 0; left: 0; width: 100%; height: 100%; text-align: center; background-size: cover; background-position: center; overflow: hidden; }\n" ++
                               ".content { margin: auto; max-width: 800px; }\n" ++
                               ".prev, .next { cursor: pointer; position: fixed; top: 50%; width: auto; padding: 16px; margin-top: -50px; color: white; font-weight: bold; font-size: 20px; transition: 0.6s ease; border-radius: 0 3px 3px 0; user-select: none; background-color: rgba(0, 0, 0, 0.5); }\n" ++
                               ".next { right: 0; border-radius: 3px 0 0 3px; }\n" ++
                               ".prev:hover, .next:hover { background-color: rgba(0, 0, 0, 0.8); }\n" ++
                               "</style>\n" ++
                               "</head>\n<body>\n" ++
                               concatMap slideToHtml slides ++
                               "<div class=\"prev\" onclick=\"plusSlides(-1)\">❮ Anterior</div>\n" ++
                               "<div class=\"next\" onclick=\"plusSlides(1)\">Siguiente ❯</div>\n" ++
                               "<script>\n" ++
                               "var slideIndex = 1;\n" ++
                               "showSlides(slideIndex);\n" ++
                               "function plusSlides(n) { showSlides(slideIndex += n); }\n" ++
                               "function showSlides(n) {\n" ++
                               "var i;\n" ++
                               "var slides = document.getElementsByClassName(\"slide\");\n" ++
                               "if (n > slides.length) { slideIndex = 1 }\n" ++
                               "if (n < 1) { slideIndex = slides.length }\n" ++
                               "for (i = 0; i < slides.length; i++) { slides[i].style.display = \"none\"; }\n" ++
                               "slides[slideIndex - 1].style.display = \"block\"; }\n" ++
                               "</script>\n" ++
                               "</body>\n</html>"

-- Ejemplo de uso
main :: IO ()
main = do input <- readFile "slide.p5"
          let token = scanner input
          putStrLn(show token) 
          tree <- parseIO pSlides token
          putStrLn(show tree) 
          writeFile "presentation.html" (slidesToHtml tree)
