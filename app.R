library(shiny)
library(readxl)
library(xlsx)
library(tidyverse)

tutor.name <- c("Harald", "Bianca", "Anna", "Ebru", "Eva", "Martina")
OBAC <- c("POB 3", "POB 2", "POB 1")
MPH <- c("MPH 1", "MPH 2")
POM <- c("POM 21", "POM 22")
ANP <- c("ANP 21", "ANP 22")
RKH <- c("RKH")

kststelle <- list(OBAC, MPH, POM, ANP, RKH)
names(kststelle) <- c("OBAC", "MPH", "PW", "ANP", "RKH")

runApp(
  list(
    ui = fluidPage(
      titlePanel("Zeiterfassung aus monatlicher Einteilung generieren"),
      sidebarLayout(
        sidebarPanel(
          fileInput('file1', 'Wähle Monatsliste (.xlsx)',
                    accept = c(".xlsx")
          ),
          selectInput("tutor", "Wer bist du?", tutor.name),
          downloadButton("download")
        ),
        mainPanel(
          tableOutput('contents'))
      )
    ),
    server = function(input, output){
      dataInput <- reactive({
        
        req(input$file1)
        
        inFile <- input$file1
        
        df_clean <- read_excel(inFile$datapath, sheet = 1, skip = 2) %>%
          select(2, 3, 4, 8) %>%
          rename(Tutor = "Tutor*in") %>%
          filter(Tutor == tutor) %>%
          separate(Zeit, c("start", "end"), " - ") %>%
          mutate(
            start = as.numeric(gsub("30", ".5", start, fixed = T)) - 0.5, # calculate duration of VHS including 30min before actual start
            end = gsub("30", ".5", end, fixed = T),
            duration = as.numeric(end) - (as.numeric(start))
            )
            
        for (i in 1:length(kststelle)) {
          print(kststelle[[i]])
          df_clean$SG[df_clean$SG %in% kststelle[[i]]] <- names(kststelle)[i]
        }
        
        # get unique dates
        dates <- unique(df_clean$Datum)
        
        # loop through the unique dates in the data frame
        first_df <- T
        for (date in dates) {
          sub <- df_clean %>% filter(Datum == date) # subset data frame by date
          # if two VHS took place at the same date calculate overall duration and break between those
          if (nrow(sub) == 2) {
            start <- as.numeric(sub[1, ]$start) - 0.5
            end <- as.numeric(tail(sub, 1)$end)
            duration <- end - start
            
            pstart <- as.numeric(sub[1, ]$end)
            pend <- as.numeric(tail(sub, 1)$start) - 0.5
            pause <- pend - pstart
            
            Datum <- unique(sub$Datum)
          }
          # if only one VHS keep the data as it is, no break in between: pause = 0
          else if (nrow(sub) == 1) {
            start <- as.numeric(sub$start) - 0.5
            end <- as.numeric(sub$end)
            duration <- end - start
            pause <- 0
            Datum <- sub$Datum
          }
          # if multiple VHS at one day, no implementation yet, therefore 1000 as sort of a warning
          else {
            start <- as.numeric(sub$start[1]) - 0.5
            end <- as.numeric(tail(sub, 1)$end)
            duration <- 1000
            pause <- 1000
            Datum <- sub$Datum
          }
          df <- data_frame(Datum = Datum, duration_u = duration, pause_u = pause, start_new = start, end_new = end)
          # concat individual rows
          if (first_df == T) {
            last <- df
            first_df <- F
          } else {
            last <- rbind(df, last)
            unique_Dates_df <- last
          }
        }
        
        # full join to include duration of individual VHS
        joined <- df_clean %>%
          select(-Tutor, -start, -end) %>%
          full_join(unique_Dates_df, by = "Datum")
        
        # if two VHS at one day overlap they have a negative pause value
        # half of the overlap is substracted from the duration of individual VHS
        for (date in dates) {
          sub <- joined %>% filter(Datum == date)
          print(sub$pause_u[1])
          if (sub$pause_u[1] < 0) {
            split <- sub$pause_u[1] / 2
            joined$split_duration[joined$Datum == date] <- joined$duration[joined$Datum == date] + split
            joined$pause_u[joined$Datum == date] <- 0
          } else {
            joined$split_duration[joined$Datum == date] <- joined$duration[joined$Datum == date]
          }
        }
        
        # widen date frame so dates only occur once and duration for individual Kostenstellen can be seen in columns. If two VHS of the same Kostenstelle take place on one day, their duration is added to each other
        joined_wider <- pivot_wider(joined %>% select(-duration), names_from = SG, values_from = split_duration, values_fn = sum)
        
        # change order of columns
        ordered <- seq(1, ncol(joined_wider))
        ordered[2:5] <- c(4, 5, 2, 3)
        final <- joined_wider[, ordered] %>% rename(Start = start_new , Ende = end_new, Dauer = duration_u, Pause = pause_u)
        
        # change formats to time instead of decimal numbers
        for (i in 2:ncol(final)) {
          final[[i]] <- paste(floor(final[[i]]), round((final[[i]] - floor(as.numeric(final[[i]]))) * 60), sep = ":")  
        }
        
        final <- data.frame(lapply(final, function(x) {
          gsub("NA:NA", "", x)
        }))
        
        return(final)
      })
      
      output$contents <- renderTable({
        dataInput()
      })
      output$download <- downloadHandler(
        filename = function() {
          "Zeiterfassung.xlsx"
        },
        content = function(file) {
          write.xlsx(dataInput(), file, sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
        }
      )
      
      
    }
  )
)



