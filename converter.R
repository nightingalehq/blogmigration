library(brio)
library(rmarkdown)
library(yaml)
library(curl)
posts <- read_csv("hubspot-blog-export--2021-04-06.csv")
#dir.create("html")
#dir.create("md")
slugger <- function(x){gsub("https://blog.nightingalehq.ai/","",x,fixed=TRUE)}
published_posts = posts[posts$Status!="DRAFT",]

write_lines(
    paste(paste0("/",slugger(published_posts$`Post URL`)),
          paste0("https://nightingalehq.ai/blog/",slugger(published_posts$`Post URL`)),
          301)
    , "_REDIRECTS")

posts_to_convert = 1:nrow(published_posts)
#posts_to_convert = 2
lapply(posts_to_convert, function(x){ 
  row = published_posts[x,]
  base_name = slugger(row$`Post URL`)
  
  html = paste0("html/", base_name, ".html")
  md_dir = paste0("md/", base_name)
  dir.create(md_dir)
  md = paste0(md_dir, "/index.md")
  
  img_filename= gsub("^h.*/|\\*","",`row`$`Featured image URL`)
  img= gsub("\\..*$","",img_filename)
  curl_download(`row`$`Featured image URL`,
                destfile =paste0(md_dir,"/",img_filename ))
  
  metadata = list(
    title = row$`Post title`,
    author = row$Author,
    tags = strsplit(row$Tags, ",")[[1]],
    meta=row$`Meta description`,
    date=format(row$`Publish date`,"%FT%T"),
    lastmod=format(row$`Last modified date`,"%FT%T"),
    image=img,
    resources = list(list(name=img, src=img_filename,title=img)),
    slug=base_name
  )
  yaml_metadata = paste0("---\n",as.yaml(metadata),"---\n")
  
  #Cleaning
  tidy_html = row$`Post body`
  tidy_html = gsub('"//','"https://',tidy_html, fixed = TRUE  )
  tidy_html = gsub('&nbsp;',' ', tidy_html, fixed = TRUE  )
  tidy_html = gsub("<br>|<span[^>]*>|</span>|<div[^>]*>|</div>","", tidy_html,ignore.case = TRUE)
  tidy_html = gsub("<p[^>]*>","<p>", tidy_html,ignore.case = TRUE)
  tidy_html = gsub("<li[^>]*>","<li>", tidy_html,ignore.case = TRUE)
  tidy_html = gsub("<ul[^>]*>","<ul>", tidy_html,ignore.case = TRUE)
  tidy_html = gsub('(id="[^>]*)',"", tidy_html,ignore.case = TRUE)
  tidy_html = gsub('<!--more-->',"", tidy_html, fixed = TRUE )
  
  #print(tidy_html)
  # create initial md files
  html_file=write_lines(tidy_html, html)
  md_file=pandoc_convert(input=html, wd=".", from="html", to="markdown", output=md)
  
  # add yaml
  md_lines = read_file(md)
  new_md_lines = paste(yaml_metadata, "\n", md_lines)
  md_file=write_lines(new_md_lines, md)
  return(base_name)
  
  })


