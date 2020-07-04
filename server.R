#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(textshape)
library(textreadr)

library(igraph)
library(ggraph)

setwd("/Users/francescapelle/Documents/Text Analytics/Final")
speech <- read_docx("speech dictation(combined).docx")

my_df <- as.data.frame(matrix(nrow = 32, ncol = 6))

for(z in 1:6){
    for(i in 1:32){
        my_df[i,z]<- speech[i*6+z-6]
    }
}


### Calculate each question separately
library(dplyr)
library(tidyr)
library(tidytext)
library(wordcloud)
library(quanteda)
library(RColorBrewer)
library(ggplot2)
q_1 <- data_frame(line=1:32, text=my_df$V1)
q_2 <- data_frame(line=1:32, text=my_df$V2)
q_3 <- data_frame(line=1:32, text=my_df$V3)
q_4 <- data_frame(line=1:32, text=my_df$V4)
q_5 <- data_frame(line=1:32, text=my_df$V5)
q_6 <- data_frame(line=1:32, text=my_df$V6)

q_1_words <- q_1 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
q_2_words <- q_2 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
q_3_words <- q_3 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
q_4_words <- q_4 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
q_5_words <- q_5 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
q_6_words <- q_6 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)

q_1_words_tf <- cbind(q_1_words, question = rep('Question 1', each = 120))
q_2_words_tf <- cbind(q_2_words, question = rep('Question 2', each = 264))
q_3_words_tf <- cbind(q_3_words, question = rep('Question 3', each = 172))
q_4_words_tf <- cbind(q_4_words, question = rep('Question 4', each = 159))
q_5_words_tf <- cbind(q_5_words, question = rep('Question 5', each = 162))
q_6_words_tf <- cbind(q_6_words, question = rep('Question 6', each = 119))

q_words_comb <- rbind.data.frame(q_1_words_tf, q_2_words_tf, q_3_words_tf, q_4_words_tf, q_5_words_tf, q_6_words_tf)

q_tf <- q_words_comb %>%
    bind_tf_idf(word, question, line)



library(stringr)
library(ggplot2)
library(png)

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$image_1 <- renderImage({
        return(list(src = "brand.png", contentType = "image/png"))
    }, deleteFile = FALSE)
    
    output$image_2 <- renderImage({
        return(list(src = "qrr.png", contentType = "image/png"))
    }, deleteFile = FALSE)
    
    output$question <- renderText({
        input$question
        })
    
    output$frequency <- renderPlot({
        if(input$question_3 == 'Question 1'){
            frequency_q_1 <- q_1 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                count(word, sort=TRUE)
            frequency_q_1 %>%
                top_n(input$slider_2) %>%
                mutate(word = reorder(word, n)) %>%
                ggplot(aes(word, n)) +
                geom_col() +
                labs(y= "Q1", x=NULL) +
                coord_flip()}
        
        else if(input$question_3 == 'Question 2'){
            frequency_q_2 <- q_2 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                count(word, sort=TRUE)
            frequency_q_2 %>%
                top_n(input$slider_2) %>%
                mutate(word = reorder(word, n)) %>%
                ggplot(aes(word, n)) +
                geom_col() +
                labs(y= "Q2", x=NULL) +
                coord_flip()}
        
        else if(input$question_3 == 'Question 3'){
            frequency_q_3 <- q_3 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                count(word, sort=TRUE)
            
            frequency_q_3 %>%
                top_n(input$slider_2) %>%
                mutate(word = reorder(word, n)) %>%
                ggplot(aes(word, n)) +
                geom_col() +
                labs(y= "Q3", x=NULL) +
                coord_flip()}
        
        else if(input$question_3 == 'Question 4'){
            frequency_q_4 <- q_4 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                count(word, sort=TRUE)
            
            frequency_q_4 %>%
                top_n(input$slider_2) %>%
                mutate(word = reorder(word, n)) %>%
                ggplot(aes(word, n)) +
                geom_col() +
                labs(y= "Q4", x=NULL) +
                coord_flip()}
        
        else if(input$question_3 == 'Question 5'){
            frequency_q_5 <- q_5 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                count(word, sort=TRUE)
            
            frequency_q_5 %>%
                top_n(input$slider_2) %>%
                mutate(word = reorder(word, n)) %>%
                ggplot(aes(word, n)) +
                geom_col() +
                labs(y= "Q5", x=NULL) +
                coord_flip()}
        
        else if(input$question_3 == 'Question 6'){
            frequency_q_6 <- q_6 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                count(word, sort=TRUE)
            
            frequency_q_6 %>%
                top_n(input$slider_2) %>%
                mutate(word = reorder(word, n)) %>%
                ggplot(aes(word, n)) +
                geom_col() +
                labs(y= "Q6", x=NULL) +
                coord_flip()}
        })
    
    output$question_Exp <- renderText({
        if(input$question == "Question 1"){print("Please describe your dietary distractions?")}
        else if(input$question == "Question 2"){print("Please describe your ideal dinner including food, drinks, company, and desert?")}
        else if(input$question == "Question 3"){print("How do you prepare(cook, buy, etc) your favorite meal?")}
        else if(input$question == "Question 4"){print("Do you usually cook or eat outside and explain why?")}
        else if(input$question == "Question 5"){print("How do you think about organic food?")}
        else if(input$question == "Question 6"){print("Would you be interested in signing up to meal.app?")}
    })
    
    output$DTM <- renderPrint({
        if(input$question == 'Question 1'){
            DTM_q_1 <- q_1 %>%
                group_by(line)%>%
                unnest_tokens(word, text)%>%
                count(word, sort=TRUE)%>%
                cast_dtm(line, word, n)
            DTM_q_1}
        
        else if(input$question == 'Question 2'){
            DTM_q_2 <- q_2 %>%
                group_by(line)%>%
                unnest_tokens(word, text)%>%
                count(word, sort=TRUE)%>%
                cast_dtm(line, word, n)
            DTM_q_2}
        
        else if(input$question == 'Question 3'){
            DTM_q_3 <- q_3 %>%
                group_by(line)%>%
                unnest_tokens(word, text)%>%
                count(word, sort=TRUE)%>%
                cast_dtm(line, word, n)
            DTM_q_3}
        
        else if(input$question == 'Question 4'){
            DTM_q_4 <- q_4 %>%
                group_by(line)%>%
                unnest_tokens(word, text)%>%
                count(word, sort=TRUE)%>%
                cast_dtm(line, word, n)
            DTM_q_4}
        
        else if(input$question == 'Question 5'){
            DTM_q_5 <- q_5 %>%
                group_by(line)%>%
                unnest_tokens(word, text)%>%
                count(word, sort=TRUE)%>%
                cast_dtm(line, word, n)
            DTM_q_5}
        
        else if(input$question == 'Question 6'){
            DTM_q_6 <- q_6 %>%
                group_by(line)%>%
                unnest_tokens(word, text)%>%
                count(word, sort=TRUE)%>%
                cast_dtm(line, word, n)
            DTM_q_6}
    })
    
    output$sentiment <- renderPrint({
        if(input$question == 'Question 1'){
            sentiment_q_1 <- q_1 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>%
                inner_join(get_sentiments("afinn")) %>%
                summarise(sum(value))
            sentiment_q_1}
        
        else if(input$question == 'Question 2'){
            sentiment_q_2 <- q_2 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>%
                inner_join(get_sentiments("afinn")) %>%
                summarise(sum(value))
            sentiment_q_2}
        
        else if(input$question == 'Question 3'){
            sentiment_q_3 <- q_3 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>%
                inner_join(get_sentiments("afinn")) %>%
                summarise(sum(value))
            sentiment_q_3}
        
        else if(input$question == 'Question 4'){
            sentiment_q_4 <- q_4 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>%
                inner_join(get_sentiments("afinn")) %>%
                summarise(sum(value))
            sentiment_q_4}
        
        else if(input$question == 'Question 5'){
            sentiment_q_5 <- q_5 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>%
                inner_join(get_sentiments("afinn")) %>%
                summarise(sum(value))
            sentiment_q_5}
        
        else if(input$question == 'Question 6'){
            sentiment_q_6 <- q_6 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>%
                inner_join(get_sentiments("afinn")) %>%
                summarise(sum(value))
            sentiment_q_6}
    })
    
    output$s_plot <- renderPlot({
        if(input$question == 'Question 1'){
            afinn_1 <- q_1_words_tf %>%
                inner_join(get_sentiments("afinn"))%>%
                group_by(index=line) %>% 
                summarise(sentiment=sum(value)) %>%
                mutate(method="AFINN")
            
            bing_and_nrc <- bind_rows(
                q_1_words_tf%>%
                    inner_join(get_sentiments("bing"))%>%
                    mutate(method = "Bing et al."),
                q_1_words_tf %>%
                    inner_join(get_sentiments("nrc") %>%
                                   filter(sentiment %in% c("positive", "negative"))) %>%
                    mutate(method = "NRC")) %>%
                count(method, index=line, sentiment) %>%
                spread(sentiment, n, fill=0) %>%
                mutate(sentiment = positive-negative)
            
            bind_rows(afinn_1, bing_and_nrc) %>%
                ggplot(aes(index, sentiment, fill=method))+
                geom_col(show.legend=FALSE)+
                facet_wrap(~method, ncol =1, scales= "free_y")}
        
        else if(input$question == 'Question 2'){
            afinn_2 <- q_2_words_tf %>%
                inner_join(get_sentiments("afinn"))%>%
                group_by(index=line) %>% 
                summarise(sentiment=sum(value)) %>%
                mutate(method="AFINN")
            
            bing_and_nrc <- bind_rows(
                q_2_words_tf%>%
                    inner_join(get_sentiments("bing"))%>%
                    mutate(method = "Bing et al."),
                q_2_words_tf %>%
                    inner_join(get_sentiments("nrc") %>%
                                   filter(sentiment %in% c("positive", "negative"))) %>%
                    mutate(method = "NRC")) %>%
                count(method, index=line, sentiment) %>%
                spread(sentiment, n, fill=0) %>%
                mutate(sentiment = positive-negative)
            
            bind_rows(afinn_2, bing_and_nrc) %>%
                ggplot(aes(index, sentiment, fill=method))+
                geom_col(show.legend=FALSE)+
                facet_wrap(~method, ncol =1, scales= "free_y")}
        
        else if(input$question == 'Question 3'){
            afinn_3 <- q_3_words_tf %>%
                inner_join(get_sentiments("afinn"))%>%
                group_by(index=line) %>% 
                summarise(sentiment=sum(value)) %>%
                mutate(method="AFINN")
            
            bing_and_nrc <- bind_rows(
                q_3_words_tf%>%
                    inner_join(get_sentiments("bing"))%>%
                    mutate(method = "Bing et al."),
                q_3_words_tf %>%
                    inner_join(get_sentiments("nrc") %>%
                                   filter(sentiment %in% c("positive", "negative"))) %>%
                    mutate(method = "NRC")) %>%
                count(method, index=line, sentiment) %>%
                spread(sentiment, n, fill=0) %>%
                mutate(sentiment = positive-negative)
            
            bind_rows(afinn_3, bing_and_nrc) %>%
                ggplot(aes(index, sentiment, fill=method))+
                geom_col(show.legend=FALSE)+
                facet_wrap(~method, ncol =1, scales= "free_y")}
        
        else if(input$question == 'Question 4'){
            afinn_4 <- q_4_words_tf %>%
                inner_join(get_sentiments("afinn"))%>%
                group_by(index=line) %>% 
                summarise(sentiment=sum(value)) %>%
                mutate(method="AFINN")
            
            bing_and_nrc <- bind_rows(
                q_4_words_tf%>%
                    inner_join(get_sentiments("bing"))%>%
                    mutate(method = "Bing et al."),
                q_4_words_tf %>%
                    inner_join(get_sentiments("nrc") %>%
                                   filter(sentiment %in% c("positive", "negative"))) %>%
                    mutate(method = "NRC")) %>%
                count(method, index=line, sentiment) %>%
                spread(sentiment, n, fill=0) %>%
                mutate(sentiment = positive-negative)
            
            bind_rows(afinn_4, bing_and_nrc) %>%
                ggplot(aes(index, sentiment, fill=method))+
                geom_col(show.legend=FALSE)+
                facet_wrap(~method, ncol =1, scales= "free_y")}
        
        else if(input$question == 'Question 5'){
            afinn_5 <- q_5_words_tf %>%
                inner_join(get_sentiments("afinn"))%>%
                group_by(index=line) %>% 
                summarise(sentiment=sum(value)) %>%
                mutate(method="AFINN")
            
            bing_and_nrc <- bind_rows(
                q_5_words_tf%>%
                    inner_join(get_sentiments("bing"))%>%
                    mutate(method = "Bing et al."),
                q_5_words_tf %>%
                    inner_join(get_sentiments("nrc") %>%
                                   filter(sentiment %in% c("positive", "negative"))) %>%
                    mutate(method = "NRC")) %>%
                count(method, index=line, sentiment) %>%
                spread(sentiment, n, fill=0) %>%
                mutate(sentiment = positive-negative)
            
            bind_rows(afinn_5, bing_and_nrc) %>%
                ggplot(aes(index, sentiment, fill=method))+
                geom_col(show.legend=FALSE)+
                facet_wrap(~method, ncol =1, scales= "free_y")}
        
        else if(input$question == 'Question 6'){
            afinn_6 <- q_6_words_tf %>%
                inner_join(get_sentiments("afinn"))%>%
                group_by(index=line) %>% 
                summarise(sentiment=sum(value)) %>%
                mutate(method="AFINN")
            
            bing_and_nrc <- bind_rows(
                q_6_words_tf%>%
                    inner_join(get_sentiments("bing"))%>%
                    mutate(method = "Bing et al."),
                q_6_words_tf %>%
                    inner_join(get_sentiments("nrc") %>%
                                   filter(sentiment %in% c("positive", "negative"))) %>%
                    mutate(method = "NRC")) %>%
                count(method, index=line, sentiment) %>%
                spread(sentiment, n, fill=0) %>%
                mutate(sentiment = positive-negative)
            
            bind_rows(afinn_6, bing_and_nrc) %>%
                ggplot(aes(index, sentiment, fill=method))+
                geom_col(show.legend=FALSE)+
                facet_wrap(~method, ncol =1, scales= "free_y")}
    })
    output$np_plot <- renderPlot({
        if(input$question == 'Question 1'){
            tidy_q_1 <- q_1 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            tidy_q_1 %>%
                group_by(sentiment) %>%
                top_n(input$slider) %>%
                ungroup() %>%
                mutate(word=reorder(word, n)) %>%
                ggplot(aes(word, n, fill=sentiment)) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~sentiment, scales = "free_y")+
                labs(y="Contribution to sentiment", x=NULL)+
                coord_flip()}
        
        else if(input$question == 'Question 2'){
            tidy_q_2 <- q_2 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            tidy_q_2 %>%
                group_by(sentiment) %>%
                top_n(input$slider) %>%
                ungroup() %>%
                mutate(word=reorder(word, n)) %>%
                ggplot(aes(word, n, fill=sentiment)) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~sentiment, scales = "free_y")+
                labs(y="Contribution to sentiment", x=NULL)+
                coord_flip()}
        
        else if(input$question == 'Question 3'){
            tidy_q_3 <- q_3 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            tidy_q_3 %>%
                group_by(sentiment) %>%
                top_n(input$slider) %>%
                ungroup() %>%
                mutate(word=reorder(word, n)) %>%
                ggplot(aes(word, n, fill=sentiment)) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~sentiment, scales = "free_y")+
                labs(y="Contribution to sentiment", x=NULL)+
                coord_flip()}
        
        else if(input$question == 'Question 4'){
            tidy_q_4 <- q_4 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            tidy_q_4 %>%
                group_by(sentiment) %>%
                top_n(input$slider) %>%
                ungroup() %>%
                mutate(word=reorder(word, n)) %>%
                ggplot(aes(word, n, fill=sentiment)) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~sentiment, scales = "free_y")+
                labs(y="Contribution to sentiment", x=NULL)+
                coord_flip()}
        
        else if(input$question == 'Question 5'){
            tidy_q_5 <- q_5 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            tidy_q_5 %>%
                group_by(sentiment) %>%
                top_n(input$slider) %>%
                ungroup() %>%
                mutate(word=reorder(word, n)) %>%
                ggplot(aes(word, n, fill=sentiment)) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~sentiment, scales = "free_y")+
                labs(y="Contribution to sentiment", x=NULL)+
                coord_flip()}
        
        else if(input$question == 'Question 6'){
            tidy_q_6 <- q_6 %>%
                unnest_tokens(word, text) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            tidy_q_6 %>%
                group_by(sentiment) %>%
                top_n(input$slider) %>%
                ungroup() %>%
                mutate(word=reorder(word, n)) %>%
                ggplot(aes(word, n, fill=sentiment)) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~sentiment, scales = "free_y")+
                labs(y="Contribution to sentiment", x=NULL)+
                coord_flip()}
    })
    
    output$tf_plot <- renderPlot({
        if(input$question == 'Question 1'){
            q_tf %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(question) %>%
                top_n(input$slider) %>%
                filter(question == "Question 1") %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=question))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="tf-idf")+
                facet_wrap(~question, ncol=2, scales="free")+
                coord_flip()}
        
        else if(input$question == 'Question 2'){
            q_tf %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(question) %>%
                top_n(input$slider) %>%
                filter(question == "Question 2") %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=question))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="tf-idf")+
                facet_wrap(~question, ncol=2, scales="free")+
                coord_flip()}
        
        else if(input$question == 'Question 3'){
            q_tf %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(question) %>%
                top_n(input$slider) %>%
                filter(question == "Question 3") %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=question))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="tf-idf")+
                facet_wrap(~question, ncol=2, scales="free")+
                coord_flip()}
        
        else if(input$question == 'Question 4'){
            q_tf %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(question) %>%
                top_n(input$slider) %>%
                filter(question == "Question 4") %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=question))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="tf-idf")+
                facet_wrap(~question, ncol=2, scales="free")+
                coord_flip()}
        
        else if(input$question == 'Question 5'){
            q_tf %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(question) %>%
                top_n(input$slider) %>%
                filter(question == "Question 5") %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=question))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="tf-idf")+
                facet_wrap(~question, ncol=2, scales="free")+
                coord_flip()}
        
        else if(input$question == 'Question 6'){
            q_tf %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(question) %>%
                top_n(input$slider) %>%
                filter(question == "Question 6") %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=question))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="tf-idf")+
                facet_wrap(~question, ncol=2, scales="free")+
                coord_flip()}
    })
    
    output$question_2 <- renderText({
        input$question
    })
    
    output$question_Exp_2 <- renderText({
        if(input$question_2 == "Question 1"){print("Please describe your dietary distractions?")}
        else if(input$question_2 == "Question 2"){print("Please describe your ideal dinner including food, drinks, company, and desert?")}
        else if(input$question_2 == "Question 3"){print("How do you prepare(cook, buy, etc) your favorite meal?")}
        else if(input$question_2 == "Question 4"){print("Do you usually cook or eat outside and explain why?")}
        else if(input$question_2 == "Question 5"){print("How do you think about organic food?")}
        else if(input$question_2 == "Question 6"){print("Would you be interested in signing up to meal.app?")}
    })
    
    output$tf_plot <- renderPlot({
        if(input$question == 'Question 1'){
            q_tf %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(question) %>%
                top_n(input$slider) %>%
                filter(question == "Question 1") %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=question))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="tf-idf")+
                facet_wrap(~question, ncol=2, scales="free")+
                coord_flip()}
        
        else if(input$question == 'Question 2'){
            q_tf %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(question) %>%
                top_n(input$slider) %>%
                filter(question == "Question 2") %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=question))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="tf-idf")+
                facet_wrap(~question, ncol=2, scales="free")+
                coord_flip()}
        
        else if(input$question == 'Question 3'){
            q_tf %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(question) %>%
                top_n(input$slider) %>%
                filter(question == "Question 3") %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=question))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="tf-idf")+
                facet_wrap(~question, ncol=2, scales="free")+
                coord_flip()}
        
        else if(input$question == 'Question 4'){
            q_tf %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(question) %>%
                top_n(input$slider) %>%
                filter(question == "Question 4") %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=question))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="tf-idf")+
                facet_wrap(~question, ncol=2, scales="free")+
                coord_flip()}
        
        else if(input$question == 'Question 5'){
            q_tf %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(question) %>%
                top_n(input$slider) %>%
                filter(question == "Question 5") %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=question))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="tf-idf")+
                facet_wrap(~question, ncol=2, scales="free")+
                coord_flip()}
        
        else if(input$question == 'Question 6'){
            q_tf %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(question) %>%
                top_n(input$slider) %>%
                filter(question == "Question 6") %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=question))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="tf-idf")+
                facet_wrap(~question, ncol=2, scales="free")+
                coord_flip()}
    })
    
    output$question_3 <- renderText({
        input$question_3
    })
    
    output$question_Exp_3 <- renderText({
        if(input$question_3 == "Question 1"){print("Please describe your dietary distractions?")}
        else if(input$question_3 == "Question 2"){print("Please describe your ideal dinner including food, drinks, company, and desert?")}
        else if(input$question_3 == "Question 3"){print("How do you prepare(cook, buy, etc) your favorite meal?")}
        else if(input$question_3 == "Question 4"){print("Do you usually cook or eat outside and explain why?")}
        else if(input$question_3 == "Question 5"){print("How do you think about organic food?")}
        else if(input$question_3 == "Question 6"){print("Would you be interested in signing up to meal.app?")}
    })
    
    output$wordcloud <- renderPlot({
        if(input$question_3 == 'Question 1'){
            q_1 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                count(word, sort=TRUE) %>%
                with(wordcloud(word, n, min.freq = input$min_freq, max.words = input$max_words))}
        
        else if(input$question_3 == 'Question 2'){
            q_2 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                count(word, sort=TRUE) %>%
                with(wordcloud(word, n, min.freq = input$min_freq, max.words = input$max_words))}
        
        else if(input$question_3 == 'Question 3'){
            q_3 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                count(word, sort=TRUE) %>%
                with(wordcloud(word, n, min.freq = input$min_freq, max.words = input$max_words))}
        
        else if(input$question_3 == 'Question 4'){
            q_4 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                count(word, sort=TRUE) %>%
                with(wordcloud(word, n, min.freq = input$min_freq, max.words = input$max_words))}
        
        else if(input$question_3 == 'Question 5'){
            q_5 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                count(word, sort=TRUE) %>%
                with(wordcloud(word, n, min.freq = input$min_freq, max.words = input$max_words))}
        
        else if(input$question_3 == 'Question 6'){
            q_6 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>% 
                count(word, sort=TRUE) %>%
                with(wordcloud(word, n, min.freq = input$min_freq, max.words = input$max_words))}
    })
    
    output$n_plot <- renderPlot({
        if(input$question_2 == 'Question 1'){
            if(input$`n-gram` == "2"){
                bigrams_1 <- q_1 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=2) %>%
                    separate(ngram, c("word1", "word2"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words)
                
                bigram_counts_1 <- bigrams_1 %>%
                    count(word1, word2, sort = TRUE)
                
                bigram_graph_1 <- bigram_counts_1 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
    
                ggraph(bigram_graph_1, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}
            
            else if(input$`n-gram` == "3"){
                bigrams_1 <- q_1 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=3) %>%
                    separate(ngram, c("word1", "word2", "word3"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words) %>%
                    filter(!word3 %in% stop_words)
                
                bigram_counts_1 <- bigrams_1 %>%
                    count(word1, word2, word3, sort = TRUE)
                
                bigram_graph_1 <- bigram_counts_1 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_1, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}
            
            else if(input$`n-gram` == "4"){
                bigrams_1 <- q_1 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=4) %>%
                    separate(ngram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words) %>%
                    filter(!word3 %in% stop_words) %>%
                    filter(!word4 %in% stop_words)
                
                bigram_counts_1 <- bigrams_1 %>%
                    count(word1, word2, word3, word4, sort = TRUE)
                
                bigram_graph_1 <- bigram_counts_1 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_1, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}}
        
        else if(input$question_2 == 'Question 2'){
            if(input$`n-gram` == "2"){
                bigrams_2 <- q_2 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=2) %>%
                    separate(ngram, c("word1", "word2"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words)
                
                bigram_counts_2 <- bigrams_2 %>%
                    count(word1, word2, sort = TRUE)
                
                bigram_graph_2 <- bigram_counts_2 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_2, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}
            
            else if(input$`n-gram` == "3"){
                bigrams_2 <- q_2 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=3) %>%
                    separate(ngram, c("word1", "word2", "word3"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words) %>%
                    filter(!word3 %in% stop_words)
                
                bigram_counts_2 <- bigrams_2 %>%
                    count(word1, word2, word3, sort = TRUE)
                
                bigram_graph_2 <- bigram_counts_2 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_2, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}
            
            else if(input$`n-gram` == "4"){
                bigrams_2 <- q_2 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=4) %>%
                    separate(ngram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words) %>%
                    filter(!word3 %in% stop_words) %>%
                    filter(!word4 %in% stop_words)
                
                bigram_counts_2 <- bigrams_2 %>%
                    count(word1, word2, word3, word4, sort = TRUE)
                
                bigram_graph_2 <- bigram_counts_2 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_2, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}}
        
        else if(input$question_2 == 'Question 3'){
            if(input$`n-gram` == "2"){
                bigrams_3 <- q_3 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=2) %>%
                    separate(ngram, c("word1", "word2"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words)
                
                bigram_counts_3 <- bigrams_3 %>%
                    count(word1, word2, sort = TRUE)
                
                bigram_graph_3 <- bigram_counts_3 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_3, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}
            
            else if(input$`n-gram` == "3"){
                bigrams_3 <- q_3 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=3) %>%
                    separate(ngram, c("word1", "word2", "word3"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words) %>%
                    filter(!word3 %in% stop_words)
                
                bigram_counts_3 <- bigrams_3 %>%
                    count(word1, word2, word3, sort = TRUE)
                
                bigram_graph_3 <- bigram_counts_3 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_3, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}
            
            else if(input$`n-gram` == "4"){
                bigrams_3 <- q_3 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=4) %>%
                    separate(ngram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words) %>%
                    filter(!word3 %in% stop_words) %>%
                    filter(!word4 %in% stop_words)
                
                bigram_counts_3 <- bigrams_3 %>%
                    count(word1, word2, word3, word4, sort = TRUE)
                
                bigram_graph_3 <- bigram_counts_3 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_3, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}}
        
        else if(input$question_2 == 'Question 4'){
            if(input$`n-gram` == "2"){
                bigrams_4 <- q_4 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=2) %>%
                    separate(ngram, c("word1", "word2"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words)
                
                bigram_counts_4 <- bigrams_4 %>%
                    count(word1, word2, sort = TRUE)
                
                bigram_graph_4 <- bigram_counts_4 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_4, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}
            
            else if(input$`n-gram` == "3"){
                bigrams_4 <- q_4 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=3) %>%
                    separate(ngram, c("word1", "word2", "word3"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words) %>%
                    filter(!word3 %in% stop_words)
                
                bigram_counts_4 <- bigrams_4 %>%
                    count(word1, word2, word3, sort = TRUE)
                
                bigram_graph_4 <- bigram_counts_4 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_4, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}
            
            else if(input$`n-gram` == "4"){
                bigrams_4 <- q_4 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=4) %>%
                    separate(ngram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words) %>%
                    filter(!word3 %in% stop_words) %>%
                    filter(!word4 %in% stop_words)
                
                bigram_counts_4 <- bigrams_4 %>%
                    count(word1, word2, word3, word4, sort = TRUE)
                
                bigram_graph_4 <- bigram_counts_4 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_4, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}}
        
        else if(input$question_2 == 'Question 5'){
            if(input$`n-gram` == "2"){
                bigrams_5 <- q_5 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=2) %>%
                    separate(ngram, c("word1", "word2"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words)
                
                bigram_counts_5 <- bigrams_5 %>%
                    count(word1, word2, sort = TRUE)
                
                bigram_graph_5 <- bigram_counts_5 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_5, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}
            
            else if(input$`n-gram` == "3"){
                bigrams_5 <- q_5 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=3) %>%
                    separate(ngram, c("word1", "word2", "word3"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words) %>%
                    filter(!word3 %in% stop_words)
                
                bigram_counts_5 <- bigrams_5 %>%
                    count(word1, word2, word3, sort = TRUE)
                
                bigram_graph_5 <- bigram_counts_5 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_5, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}
            
            else if(input$`n-gram` == "4"){
                bigrams_5 <- q_5 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=4) %>%
                    separate(ngram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words) %>%
                    filter(!word3 %in% stop_words) %>%
                    filter(!word4 %in% stop_words)
                
                bigram_counts_5 <- bigrams_5 %>%
                    count(word1, word2, word3, word4, sort = TRUE)
                
                bigram_graph_5 <- bigram_counts_5 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_5, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}}
        
        else if(input$question_2 == 'Question 6'){
            if(input$`n-gram` == "2"){
                bigrams_6 <- q_6 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=2) %>%
                    separate(ngram, c("word1", "word2"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words)
                
                bigram_counts_6 <- bigrams_6 %>%
                    count(word1, word2, sort = TRUE)
                
                bigram_graph_6 <- bigram_counts_6 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_6, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}
            
            else if(input$`n-gram` == "3"){
                bigrams_6 <- q_6 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=3) %>%
                    separate(ngram, c("word1", "word2", "word3"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words) %>%
                    filter(!word3 %in% stop_words)
                
                bigram_counts_6 <- bigrams_6 %>%
                    count(word1, word2, word3, sort = TRUE)
                
                bigram_graph_6 <- bigram_counts_6 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_6, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}
            
            else if(input$`n-gram` == "4"){
                bigrams_6 <- q_6 %>%
                    unnest_tokens(ngram, text, token = "ngrams", n=4) %>%
                    separate(ngram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
                    filter(!word1 %in% stop_words) %>%
                    filter(!word2 %in% stop_words) %>%
                    filter(!word3 %in% stop_words) %>%
                    filter(!word4 %in% stop_words)
                
                bigram_counts_6 <- bigrams_6 %>%
                    count(word1, word2, word3, word4, sort = TRUE)
                
                bigram_graph_6 <- bigram_counts_6 %>%
                    filter(n>1) %>%
                    graph_from_data_frame()
                
                ggraph(bigram_graph_6, layout = "fr") +
                    geom_edge_link()+
                    geom_node_point()+
                    geom_node_text(aes(label=name), vjust =1, hjust=1)}}
    })
    
    output$nb <- renderTable({
        q_t <- apply(my_df, 1, paste, collapse = ",")
        
        df.dfm <- dfm(corpus(q_t), tolower = TRUE)
        df.dfm <- dfm_trim(df.dfm, min_termfreq = 2, min_docfreq = 1)
        df.dfm <- dfm_weight(df.dfm)

        df.dfm.train<-df.dfm[1:25,]
        df.dfm.test<-df.dfm[26:32,]
        
        NB_classifier <- textmodel_nb(df.dfm.train, c(1,0,1,1,1,
                                                      1,0,1,1,1,
                                                      1,0,1,1,1,
                                                      1,0,1,1,1,
                                                      0,0,0,1,1))
        nb <- data.frame(NB_classifier$PwGc[,31:39])
        nb
    })
    
    output$predict <- renderTable({
        q_t <- apply(my_df, 1, paste, collapse = ",")
        
        df.dfm <- dfm(corpus(q_t), tolower = TRUE)
        df.dfm <- dfm_trim(df.dfm, min_termfreq = 2, min_docfreq = 1)
        df.dfm <- dfm_weight(df.dfm)
        
        df.dfm.train<-df.dfm[1:25,]
        df.dfm.test<-df.dfm[26:32,]
        
        NB_classifier <- textmodel_nb(df.dfm.train, c(1,0,1,1,1,
                                                      1,0,1,1,1,
                                                      1,0,1,1,1,
                                                      1,0,1,1,1,
                                                      0,0,0,1,1))
        pred <- predict(NB_classifier, df.dfm.test)
        pred <- t(data.frame(pred))
        pred
    })

})

