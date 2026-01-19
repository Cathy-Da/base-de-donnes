# API wikipedia

wikipedia <-function(langue, titre) {
      url <-paste("https://", langue, ".wikipedia.org/w/api.php", sep = "")
      reponse <-GET(url, query = list(
              action = "parse",
              page = titre,
              format = "json",
              prop = "text",
              formatversion = 2
)
)
  
  if (status_code(reponse) == 200) {
    texte_wikipedia <-fromJSON(content(reponse, "text", encoding = "UTF-8"))
    html <-read_html(texte_wikipedia$parse$text)
    xml_remove(xml_find_all(html, ".//img | .//figure"))
    return(html_text(html))
}
      
  else {
    warning("Erreur:", titre)
    return(NA)
}
}
