#### Set Up ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tune, workflows, dials, hardhat, parsnip, textrecipes, rsample, netstat,netstat,rvest,purrr,tm,stringr,ggplot2,dplyr,tidytext,syuzhet,textdata, tidyr, data.table,WriteXLS,wordcloud,ggwordcloud,gganimate,gifski,png, topicmodels)
dfTranscript <- readRDS("~/YoutubeTranscript/YoutubeTranscript/dfTranscript.Rda") 
funca? 
#### Scrap ----
# https://stackoverflow.com/questions/51014205/automating-opening-transcript-for-youtube-automatic-generated-captions
rs_driver_object <- rsDriver(browser = 'chrome',
                             chromever = '100.0.4896.60',
                             verbose = FALSE,
                             port = free_port())

# create a client object
remDr <- rs_driver_object$client

remDr$navigate('https://www.youtube.com/watch?v=9Rll3mxpfN0')
# stop autoplay
play_button <- remDr$findElement(using = 'class', value = "ytp-play-button")
play_button$clickElement()

# # activate subtitles
# subtitle_button <- remDr$findElement(using = "class", value = "ytp-subtitles-button")
# subtitle_button$clickElement()
# textTitle <- unlist(remDr$findElement(using = 'xpath' ,"//yt-formatted-string[@class = 'style-scope ytd-video-primary-info-renderer']")$getElementText())
# textViews <- unlist(remDr$findElement(using = 'xpath' ,'//*[@id="count"]/ytd-video-view-count-renderer/span[1]')$getElementText())
# textDate <- unlist(remDr$findElement(using = 'css' ,".ytd-video-primary-info-renderer")$getElementText())
# textDate <- unlist(remDr$findElement(using = 'css' ,".ytd-video-primary-info-renderer:nth-child(2)")$getElementText())
# textDate <- unlist(remDr$findElement(using = 'xpath' ,"//span yt-formatted-string[@class = 'style-scope ytd-video-primary-info-renderer']")$getElementText())
textViews <- unlist(remDr$findElement(using = 'xpath' ,"//ytd-video-view-count-renderer[@class='style-scope ytd-video-primary-info-renderer']")$getElementText())
textTitle <- unlist(remDr$findElement(using = 'xpath' ,"//yt-formatted-string[@class = 'style-scope ytd-video-primary-info-renderer']")$getElementText())


#tocar 3 puntos
toca3puntos <- remDr$findElement(using = 'xpath', "//button[@aria-label = 'More actions']")
toca3puntos$clickElement()

showTranscript <- remDr$findElement(using = 'xpath', "//tp-yt-paper-listbox[@id = 'items']/ytd-menu-service-item-renderer")
showTranscript$clickElement()

Transcript <- remDr$findElement(using = 'xpath' ,"//div[@id = 'segments-container']")
text <- Transcript$getElementText() %>% unlist()

#### Channel
remDr$navigate('https://www.youtube.com/c/GavinWebber/videos')

# Scroll hasta abajo
webElem <- remDr$findElement("css", "body")
for (i in c(1:25)) {
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(1)
}

# Look up links
gridLinks <- remDr$findElements(using = 'css selector' ,"#video-title")
# resHeaders <- unlist(lapply(gridLinks, function(x) {x$getElementText()}))
# resHeaders
resHref <- unlist(lapply(gridLinks, function(x) {x$getElementAttribute("href")}))
View(resHref)

#### Función 1x1
get_transcript <- function(url) {
  tryCatch( {
    remDr$navigate(url)
    play_button <- remDr$findElement(using = 'class', value = "ytp-play-button")$clickElement()
    Sys.sleep(1)
    textTitle <- unlist(remDr$findElement(using = 'xpath' ,"//yt-formatted-string[@class = 'style-scope ytd-video-primary-info-renderer']")$getElementText())
    Sys.sleep(1)
    textViews <- unlist(remDr$findElement(using = 'xpath' ,"//ytd-video-view-count-renderer[@class='style-scope ytd-video-primary-info-renderer']")$getElementText())
    Sys.sleep(1)
    toca3puntos <- remDr$findElement(using = 'xpath', "//button[@aria-label = 'More actions']")$clickElement()
    Sys.sleep(1)
    showTranscript <- remDr$findElement(using = 'xpath', "//tp-yt-paper-listbox[@id = 'items']/ytd-menu-service-item-renderer")$clickElement()
    Sys.sleep(1)
    Transcript <- remDr$findElement(using = 'xpath' ,"//div[@id = 'segments-container']")
    textTranscript <- Transcript$getElementText() %>% unlist()
    dfTranscript <- data.frame(
      titleCol = textTitle,
      viewsCol = textViews,
      textCol = textTranscript,
      stringsAsFactors = FALSE)},
  error = function(e) NULL)
return(dfTranscript)
}

# get_transcript('https://www.youtube.com/watch?v=9Rll3mxpfN0')
# results <- map_df(resHref[1:2],get_transcript)
dfTranscript <- data.frame()
dfTranscript <- bind_rows(dfTranscript,map(resHref,get_transcript))
# WriteXLS(newsallDF, ExcelFileName = 'dfTranscript.xls')
saveRDS(dfTranscript, file = "dfTranscript.Rda") #Guardamos el df solamente

#### Data Wrangling ----
# cheese list http://www.nourishinteractive.com/healthy-living/free-nutrition-articles/110-list-cheeses
dfTranscriptA <- dfTranscript
dfTranscriptA$textCol <- gsub("[0-9]+", "", dfTranscript$textCol) 
dfTranscriptA$textCol <- gsub(":", "", dfTranscriptA$textCol) 
dfTranscriptA$viewsCol <- gsub(" views", "",dfTranscriptA$viewsCol)
dfTranscriptA$viewsCol <- gsub(",", "",dfTranscriptA$viewsCol)
dfTranscriptA$viewsCol <- as.numeric(dfTranscriptA$viewsCol)

dfTranscriptB <- dfTranscriptA %>% mutate(group=if_else(str_detect(dfTranscriptA$titleCol,"Ask|(?i)cheeseman"),"Ask the Cheeseman",
                                                        if_else(str_detect(dfTranscriptA$titleCol,"(?i)how|(?i)make|(?i)making|Day"), "Cheesemaking", 
                                                                if_else(str_detect(dfTranscriptA$titleCol,"(?i)test|(?i)taste"), "Testings", "Others"))))
dfTranscriptC <- dfTranscriptB 
dfTranscriptB <- dfTranscriptB %>% filter(!group %in% c('Others'))

#### Classification ----
set.seed(1234)
multinews_split <- initial_split(dfTranscriptB, strata = group)
multinews_train <- training(multinews_split)
multinews_test <- testing(multinews_split)

multinews_train %>%
  group_by(group) %>% summarise(n = n()) %>%
  mutate(share = n/sum(n)) %>%
  select(group, n, share) %>% arrange(share)

multinews_rec <-
  recipe(group ~ textCol,
         data = multinews_train) %>%
  step_tokenize(textCol) %>%
  step_stopwords(textCol, language = 'en') %>%
  step_tokenfilter(textCol, max_tokens = 1e3, min_times = 100) %>%
  step_tfidf(textCol)

multinews_folds <- vfold_cv(multinews_train)

multi_spec <- multinom_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

multi_spec

#cargamos info de sparce encoding
sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")
smaller_lambda <- grid_regular(penalty(range = c(-5, 0)), levels = 20)

multi_lasso_wf <- workflow() %>%
  add_recipe(multinews_rec, blueprint = sparse_bp) %>%
  add_model(multi_spec)

multi_lasso_wf

multi_lasso_rs <- tune_grid(
  multi_lasso_wf,
  multinews_folds,
  grid = smaller_lambda,
  control = control_resamples(save_pred = TRUE)
)

multi_lasso_rs

best_acc <- multi_lasso_rs %>%
  show_best("accuracy")

best_acc

choose_acc <- multi_lasso_rs %>%
  select_by_pct_loss(metric = "accuracy", -penalty)

multi_lasso_rs %>%
  collect_predictions() %>%
  filter(penalty == best_acc$penalty) %>%
  filter(id == "Fold01") %>%
  conf_mat(group, .pred_class) %>%
  autoplot(type = "heatmap") +
  scale_y_discrete(labels = function(x) str_wrap(x, 20)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 20))

finalMulti_wf0 <- finalize_workflow(multi_lasso_wf, choose_acc)
#En este momento aplicamos el wf a nuestros datos, por un lado determinamos el modelo con nuestro training set y dps lo evaluamos con el test set. En final fitted están todos los resultados
finalMulti_wf <- last_fit(finalMulti_wf0, multinews_split)
#Nos tiraba un error al tratar de usar el fit del final fitted a través dell pull_wf_fit y dps predict. Usamos como alternativa fit sobre el training set https://stackoverflow.com/questions/63334046/how-to-simulate-last-fit-using-fit-in-tidymodels
finalMulti_fitted <- finalMulti_wf0 %>% fit(training(multinews_split))
dfTranscriptC <- predict(finalMulti_fitted,dfTranscriptC) %>%
  bind_cols(dfTranscriptC)
saveRDS(news_bind, file = "videoPredMulti.Rda") #Guardamos el df solamente


#LDA ----
delete_words <- c('cheese','milk', 'yeah', 'cheeses',"cheese's",'video','lot')
custom_stop_words <- bind_rows(stop_words,
                               tibble(word = stopwords::stopwords("en", source = "stopwords-iso"),
                                      lexicon = "custom"))
unnestWords2 <- dfTranscriptA %>%
  unnest_tokens(word, textCol, token = "words", to_lower = TRUE) %>%
  anti_join(custom_stop_words) %>% filter(!word %in% delete_words) # %>% left_join(countMonth)
unnestWords2$id <-seq.int(nrow(unnestWords2))
# unnestWords2 <- unnestWords2 %>% unite('id2', fecha,header,sep = '_', remove = FALSE)
WordsCountArticle <- unnestWords2 %>% 
  count(titleCol, word, sort = TRUE) %>%
  ungroup()
chapters_dtm <- WordsCountArticle %>%
  cast_dtm(titleCol, word, n)
chapters_dtm
chapters_lda <- LDA(chapters_dtm, k = 3, method = "Gibbs", control = list(seed = 1234))
chapters_lda
chapter_topics <- tidy(chapters_lda, matrix = "beta")
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

chapters_gamma <- tidy(chapters_lda, matrix = "gamma") %>% rename(titleCol=document)
chapters_gamma
chapters_gamma <- chapters_gamma %>%
  inner_join(dfTranscriptA)
View(chapters_gamma)
class(chapters_gamma)

chapters_gamma %>% count(topic)


#AUX ----
?remDr$findElement()
//*[@id="segments-container"]/ytd-transcript-segment-renderer[1]/div/yt-formatted-string
//*[@id="segments-container"]/ytd-transcript-segment-renderer[2]/div/yt-formatted-string
<yt-formatted-string class="segment-text style-scope ytd-transcript-segment-renderer" aria-hidden="true" tabindex="-1">Hi.</yt-formatted-string>

//*[@id="content"]
//*[@id="items"]/ytd-menu-service-item-renderer 
//*[@id="button"]
more_action_btn = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.XPATH, "//button[@aria-label = 'More actions']")))
more_action_btn.click()


# captions text element
caption_window <- remDr$findElement(using = "class", value = "captions-text")
//*[@id="button"]
yt-formatted-string.ytd-menu-service-item-renderer
ytd-menu-renderer.ytd-video-primary-info-renderer > yt-icon-button:nth-child(3) > button:nth-child(1)
/html/body/ytd-app/ytd-popup-container/tp-yt-iron-dropdown[2]/div/ytd-menu-popup-renderer/tp-yt-paper-listbox/ytd-menu-service-item-renderer/tp-yt-paper-item/yt-formatted-string
# retrieve plain text
text <- caption_window$getElementText()

remDr$navigate("http://www.google.com/ncr")
webElem <- remDr$findElement(using = "css", "[name = 'q']")
webElem$sendKeysToElement(list("R Cran", key = "enter"))

webElems <- remDr$findElements(using = "css selector", "h3")
resHeaders <- unlist(lapply(webElems, function(x) {x$getElementText()}))
resHeaders

remDr$navigate('https://www.youtube.com/c/GavinWebber/videos')

gridLinks <- remDr$findElements(using = 'css selector' ,"#video-title")
resHeaders <- unlist(lapply(gridLinks, function(x) {x$getElementText()}))
# resHeaders <- unlist(lapply(gridLinks, function(x) {x$findElement(using = 'css' ,"#video-title")$getElementText()}))
resHeaders

resHref <- unlist(lapply(gridLinks, function(x) {x$getElementAttribute("href")}))
resHref

remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")

remDr$navigate('https://www.youtube.com/c/GavinWebber/videos')
webElem <- remDr$findElement("css", "body")
for (i in c(1:15)) {
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(3)
  }



last_height = 0 #
repeat {   
  webElem$sendKeysToElement(list(key = "end"))
  Sys.sleep(3) #delay by 3sec to give chance to load. 
  
  # Updated if statement which breaks if we can't scroll further 
  new_height = remDr$executeScript("return document.body.scrollHeight")
  if(unlist(last_height) == unlist(new_height)) {
    break
  } else {
    last_height = new_height
  }
}

str_detect(dfTranscriptA$titleCol,"(?i)taste")
table(str_detect(dfTranscriptA$titleCol,"Ask|(?i)cheeseman"))["TRUE"]
table(str_detect(dfTranscriptA$titleCol,"(?i)how|(?i)make|(?i)making"))["TRUE"]
table(str_detect(dfTranscriptA$titleCol,"Day |Ask"))["TRUE"]
table(str_detect(dfTranscriptA$titleCol,"Ask"))["TRUE"]

table(str_detect(dfTranscriptA$titleCol,"(?i)how | (?i)make | (?i)test | (?i)tast | (?i)ask | (?i)cheeseman"))["FALSE"]

dfTranscriptB <- dfTranscriptA %>% dplyr::filter(str_detect(dfTranscriptA$titleCol,"(?i)how | (?i)making | (?i)make | (?i)test | (?i)taste | (?i)ask | (?i)cheeseman",  negate = TRUE))

