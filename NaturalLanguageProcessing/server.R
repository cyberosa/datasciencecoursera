#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tm)
library(SnowballC)
library(tidyr)
library(qdap)
library(dplyr)
library(stringr)
library(stringi)
# upload the n-gram models
bigramFreq <- readRDS("bigramFreq.rds")
trigramFreq <- readRDS("trigramFreq.rds")
quatrigramFreq <- readRDS("quatrigramFreq.rds")
totalNrDocs <- 50000
# functions available for all sessions
convert2ASCII <- function(input, print=FALSE){
    return(sapply(input, function(x) iconv(x, "latin1", "ASCII", sub = "")))
}

findFourthWord <- function(token1, token2, token3){
    token1 <- stemmer(token1)
    token2 <- stemmer(token2)
    token3 <- stemmer(token3)
    token1 <- tolower(token1)
    token2 <- tolower(token2)
    token3 <- tolower(token3)
    # try to find the frequency in trigramFreq
    found <- trigramFreq[(trigramFreq$word1 == token1) & 
                             (trigramFreq$word2 == token2) &
                             (trigramFreq$word3 == token3),]
    if (nrow(found) > 0) {
        trigramFrequency <- found[1,]$freq
    }else{ # frequency unknown
        trigramFrequency <- NULL
    }
    selectedQuatrigrams <- quatrigramFreq[(quatrigramFreq$word1 == token1) & 
                                              (quatrigramFreq$word2 == token2) &
                                              (quatrigramFreq$word3 == token3),]
    if (nrow(selectedQuatrigrams) > 0) {
        # found
        final <- head(selectedQuatrigrams[order(selectedQuatrigrams$freq, decreasing = TRUE), ])
        ifelse(is.null(trigramFrequency),return(data.frame(prediction = final$word4, prob = final$prob)),
               return(data.frame(prediction = final$word4,
                                 prob = (final$freq/trigramFrequency))))
    }else{ 
        return(data.frame(prediction = "NULL",prob = 0))
    } 
}

findThirdWord <- function(token1, token2){
    token1 <- stemmer(token1)
    token2 <- stemmer(token2)
    token1 <- tolower(token1)
    token2 <- tolower(token2)
    # try to find the frequency in bigramFreq
    found <- bigramFreq[(bigramFreq$word1 == token1) & (bigramFreq$word2 == token2),]
    if (nrow(found) > 0) {
        bigramFrequency <- found[1,]$freq
    }else{ # frequency unknown
        bigramFrequency <- NULL
    }
    selectedTrigrams <- trigramFreq[(trigramFreq$word1 == token1) & 
                                        (trigramFreq$word2 == token2),]
    if (nrow(selectedTrigrams) > 0) {
        final <- head(selectedTrigrams[order(selectedTrigrams$freq, decreasing = TRUE), ])
        ifelse(is.null(bigramFrequency),return(data.frame(prediction = final$word3, prob = final$prob)),
               return(data.frame(prediction = final$word3,
                                 prob = (final$freq/bigramFrequency))))
    }else{ 
        return(data.frame(prediction = "NULL",prob = 0))
    } 
}

# these are the less certain predictions since we do not use the unigram freq
findSecondWord <- function(token1){
    token1 <- stemmer(token1)
    token1 <- tolower(token1)
    selectedBigrams <- bigramFreq[(bigramFreq$word1 == token1),]
    if (nrow(selectedBigrams) > 0) {
        final <- head(selectedBigrams[order(selectedBigrams$freq, decreasing = TRUE), ])
        return(data.frame(prediction = final$word2, prob = final$prob))
    }else{ 
        return(data.frame(prediction = "NULL",prob = 0))
    }   
}

# token can be a  unigram,bigram or a trigram, with n we indicate it
# the frequency of the given tokens
# it returns the predicted word
findNextWord <- function(token1, token2 = NULL, token3 = NULL){
    if (is.null(token2) & is.null(token3)) { # unigram
        return(findSecondWord(token1))
    }else if (is.null(token3)) { # bigram
        # search for a trigram containing these words in the same order
        found <- findThirdWord(token1, token2)
        #print(found)
        # look for a bigram beginning with token2
        ifelse(found[1,1] == "NULL", return(findSecondWord(token2)), return(found))
    }else {# trigram
        found <- findFourthWord(token1, token2, token3)
        #print(found)
        # if null look for a trigram beginning with token2 and token3
        ifelse(found[1,1] == "NULL", return(findThirdWord(token2,token3)), return(found))
    }
}

getPredictedWord <- function(word1,word2=NULL,word3=NULL, word4=NULL){
    if (is.null(word2) & is.null(word4) & is.null(word3)) { # unigram
        found <- findNextWord(word1)
    }else if (is.null(word4) & is.null(word3)) {
        found <- findNextWord(word1,word2)
    }else if (is.null(word4)) {
        found <- findNextWord(word1,word2,word3)
    }else{
        # take the last 3 words and predict the fourth
        found <- findNextWord(word2,word3,word4)
    }
    #print(found)
    if (found[1,1] == "NULL") {
        if ( !is.null(word4) & !is.null(word3)) {
            # take the first and last two words as trigram
            found <- findNextWord(word1,word3,word4)
        }
    }
    #print(found)
    if (found[1,1] == "NULL") {
        if ( !is.null(word4) & !is.null(word3)) {
            # take the last bigram and predict the third
            found <- findNextWord(word3,word4)
        }
    }
    #print(found)
    if (found[1,1] == "NULL") {
        if (is.null(word4)) {
            found <- findNextWord(word2,word3)
        }else{
            # take the second and fourth word and predict the third
            found <- findNextWord(word2,word4)
        }
    }
    return(found)
}

# we increase the frequency of the user's feedback to give more probability to her/his options
addElement <- function(token1, token2, token3, token4 = NULL){
    token1 <- stemmer(token1)
    token2 <- stemmer(token2)
    token3 <- stemmer(token3)
    token1 <- tolower(token1)
    token2 <- tolower(token2)
    token3 <- tolower(token3)
    if (is.null(token4)) { # trigram
        found <- trigramFreq[(trigramFreq$word1 == token1) & 
                                 (trigramFreq$word2 == token2) &
                                 (trigramFreq$word3 == token3),]
        if (nrow(found) > 0) {
            # increase frequency
            newFreq <- found$freq +  100
            trigramFreq[(trigramFreq$word1 == token1) & 
                            (trigramFreq$word2 == token2) &
                            (trigramFreq$word3 == token3),4] <- newFreq
            # update probability
            newProb <- newFreq / totalNrDocs
            trigramFreq[(trigramFreq$word1 == token1) & 
                            (trigramFreq$word2 == token2) &
                            (trigramFreq$word3 == token3),5] <- newProb
        } else {
            # add new trigram. Min freq is 200
            newTrigram <- data.frame(word1 = token1, word2 = token2, word3 = token3,
                                     freq = 200, prob = 200 / totalNrDocs)
            trigramFreq <- rbind(trigramFreq,newTrigram)
        }
        return(trigramFreq)
    }else{
        token4 <- stemmer(token4)
        token4 <- tolower(token4)
        # quatrigram    
        found <- quatrigramFreq[(quatrigramFreq$word1 == token1) & 
                                    (quatrigramFreq$word2 == token2) &
                                    (quatrigramFreq$word3 == token3) &
                                    (quatrigramFreq$word4 == token4),]
        if (nrow(found) > 0) {
            # increase frequency
            newFreq <- found$freq +  100
            quatrigramFreq[(quatrigramFreq$word1 == token1) & 
                               (quatrigramFreq$word2 == token2) &
                               (quatrigramFreq$word3 == token3) &
                               (quatrigramFreq$word4 == token4),5] <- newFreq
            # update probability
            newProb <- newFreq / totalNrDocs
            quatrigramFreq[(quatrigramFreq$word1 == token1) & 
                               (quatrigramFreq$word2 == token2) &
                               (quatrigramFreq$word3 == token3) &
                               (quatrigramFreq$word4 == token4),6] <- newProb
        } else {
            # add new trigram. Min freq is 2
            newQuatrigram <- data.frame(word1 = token1, word2 = token2, word3 = token3, word4 = token4,
                                        freq = 200, prob = 200 / totalNrDocs)
            quatrigramFreq <- rbind(quatrigramFreq,newQuatrigram)
        }
        return(quatrigramFreq)
    }
}
cleanAndTokenize <- function(text){
    myInput <- tolower(text)
    # remove contractions and strange characters
    myInput <- convert2ASCII(myInput)
    myInput <- replace_contraction(myInput)
    myInput <- str_replace_all(myInput, "[^[a-zA-Z] [:space:]]", "")
    myInput <- str_replace_all(myInput, "  ", " ")
    # extract tokens
    return(unlist(strsplit(myInput, split = " ")))
}

getLastWord <- function(input){
    myTokens <- cleanAndTokenize(input)
    # very strange case, a single empty string
    if (length(myTokens) > 0){
        remove <- vector()
        for (i in 1:length(myTokens)){    
            if (as.character(myTokens[i]) == "") {
                remove <- rbind(remove,myTokens[i])
            }        
        }
        myTokens <- myTokens[!myTokens %in% remove]
    }
    if (length(myTokens) > 3) {
        # take the last three words to predict the fourth
        lastFour <- tail(myTokens,4)
        predictionTable <- getPredictedWord(lastFour[1],lastFour[2],lastFour[3],lastFour[4])
        # add trigram and quatrigram to the model
        firstThree <- head(myTokens,3)
        trigramFreq <- addElement(firstThree[1],firstThree[2],firstThree[3])
        quatrigramFreq <- addElement(lastFour[1],lastFour[2],lastFour[3],lastFour[4])
    }else if (length(myTokens) == 3) {
        predictionTable <- getPredictedWord(myTokens[1],myTokens[2],myTokens[3])
        trigramFreq <- addElement(myTokens[1],myTokens[2],myTokens[3])
    }else if (length(myTokens) == 2) {
        predictionTable <- getPredictedWord(myTokens[1],myTokens[2])
    }else if (length(myTokens) == 1) {
        predictionTable <- getPredictedWord(myTokens[1])
    }else{ # no tokens?
        return(c('the', 'on', 'a'))
    }
    results <- predictionTable$prediction
    if (length(results) > 3) {
        results <- head(results,3)
    }
    #print(results)
    if (results[1] == "NULL"){
        # default prediction
        return(c('the', 'on', 'a'))
    }else{
        return(as.character(results))
    }
}

# Predict the next word
shinyServer(function(input, output,session) {
    data_to_display <- eventReactive(input$clearSentence, NULL)
    observeEvent(input$goButton, {
        # clear text of previous sessions
        output$thank <- renderText("")
      #  updateTextInput(session, "finalWord",label = "Please provide your chosen word or an alternative one:",value = "")
        isolate({
            stime <- system.time({
                results <- getLastWord(input$inputSentence)
            })[3]
            output$predictedWords <- renderDataTable({as.data.frame(results)})
            output$computationTime <- renderText(paste("prediction time (ms): ",stime))
        })
    })
    # add feedback from the user
    observeEvent(input$correctWord, {
        isolate({
            finalWord <- as.character(input$finalWord)
            #print(finalWord)
            if (finalWord != "") {
                myTokens <- cleanAndTokenize(input$inputSentence)
                #print(myTokens)
                if (length(myTokens) > 3) {
                    lastThree <- tail(myTokens,3)
             #       print("saving trigram")
                    trigramFreq <- addElement(lastThree[2],lastThree[3],finalWord)
              #      print("saving quatrigram")
                    quatrigramFreq <- addElement(lastThree[1],lastThree[2],lastThree[3],finalWord)
                }else if (length(myTokens) == 3) {
                    trigramFreq <- addElement(myTokens[2],myTokens[3],finalWord)
                }else if (length(myTokens) == 2) {
                    trigramFreq <- addElement(myTokens[1],myTokens[2],finalWord)
                }
                output$thank <- renderText("Thanks for improving the algorithm")
                updateTextInput(session, "finalWord",label = "Please provide your chosen word or an alternative one:",value = "")
            }
        })
    })
    
    observeEvent(input$clearSentence, {
        isolate({
            updateTextInput(session,"inputSentence",label = "", value = "")
            output$computationTime <- renderText("")
            output$predictedWords <- DT::renderDataTable(data_to_display())
        })
    })
})
