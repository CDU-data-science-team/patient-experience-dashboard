
output$demographics <- renderUI({
  
  fluidPage(
    
    tagList(
      
      # multi category
      # "Ethnic", "Religion", "Sexuality", "Age", "Relationship"
      
      fluidRow(
        column(4, DTOutput("ethnicity")),
        column(4, DTOutput("age")),
        column(4, DTOutput("sexualOrientation"))
      ),
      fluidRow(
        column(4, DTOutput("gender")),
        column(4, DTOutput("disability"))
      )
    )
  )
  
  # single category
  # variables "Gender",  "Disability", "Pregnant", "Baby"
  
})

output$ethnicity <- renderDT({
  
  passData()[["currentData"]] %>% 
    count(Ethnic) %>% 
    mutate(percent = round(n / sum(n) * 100, 1)) %>% 
    arrange(desc(percent)) %>% 
    mutate(Ethnic = replace_na(Ethnic, "Not stated")) %>% 
    mutate(Ethnic = recode(Ethnic, "WB" = "White",
                           "WO" = "White other",
                           "BC" = "Black Caribbean",
                           "WI" = "White Irish",
                           "O" = "Other",
                           "AI" = "Asian Indian",
                           "AP" = "Asian Pakistani",
                           "MC" = "Mixed Black Caribbean",
                           "BA" = "Black African",
                           "AO" = "Asian Other",
                           "MA" = "Mixed African",
                           "MO" = "Mixed Other",
                           "MB" = "Mixed Black",
                           "CC" = "Chinese",
                           "BO" = "Black Other",
                           "AB" = "Asian Bangladeshi",
                           "GR" = "Gypsy, Romany, Traveller",
                           "GRT" = "Gypsy, Romany, Traveller",
                           "C" = "Chinese",
                           "OB" = "Black Other"))
}, rownames = FALSE, options = list(pageLength = 20))

output$age <- renderDT({
  
  passData()[["currentData"]] %>% 
    count(Age) %>% 
    mutate(percent = round(n / sum(n) * 100, 1)) %>% 
    arrange(Age) %>% 
    mutate(Age = as.character(Age),
           Age = recode(Age,
                        "1" = "Under 12",
                        "2" = "12-17",
                        "3" = "18-25",
                        "4" = "26-39",
                        "5" = "40-64",
                        "6" = "65-79",
                        "7" = "80+",
                        "10" = "Under 6",
                        "11" = "6-8",
                        "12" = "9-11",
                        "13" = "12-17",
                        "14" = "18+"),
           Age = replace_na(Age, "Unknown"))
}, rownames = FALSE, options = list(pageLength = 20))
    

output$sexualOrientation <- renderDT({
  
  passData()[["currentData"]] %>% 
    count(Sexuality) %>% 
    mutate(percent = round(n / sum(n) * 100, 1)) %>% 
    arrange(desc(n)) %>% 
    mutate(Sexuality = recode(Sexuality,
                              "S" = "Heterosexual",
                              "L" = "Lesbian",
                              "G" = "Gay",
                              "B" = "Bisexual",
                              "O" = "Other"),
           Sexuality = replace_na(Sexuality, "Unknown"))
  
}, rownames = FALSE, options = list(pageLength = 20))

output$gender <- renderDT({
  
  passData()[["currentData"]] %>% 
    mutate(Gender = recode(Gender,
                           "F" = "Female",
                           "M" = "Male",
                           "O" = "Other",
                           "N" = "Unknown"),
           Gender = replace_na(Gender, "Unknown")) %>% 
    count(Gender) %>% 
    mutate(percent = round(n / sum(n) * 100, 1)) %>% 
    arrange(desc(n))
  
}, rownames = FALSE, options = list(pageLength = 20))

output$gender <- renderDT({
  
  passData()[["currentData"]] %>% 
    mutate(Gender = recode(Gender,
                           "F" = "Female",
                           "M" = "Male",
                           "O" = "Other",
                           "N" = "Unknown"),
           Gender = replace_na(Gender, "Unknown")) %>% 
    count(Gender) %>% 
    mutate(percent = round(n / sum(n) * 100, 1)) %>% 
    arrange(desc(n))
  
}, rownames = FALSE, options = list(pageLength = 20))


output$disability <- renderDT({
  
  passData()[["currentData"]] %>% 
    mutate(Disability = recode(Disability,
                                "N" = "No",
                                "Y" = "Yes"),
           Disability = replace_na(Disability, "Unknown")) %>% 
    count(Disability) %>% 
    mutate(percent = round(n / sum(n) * 100, 1)) %>% 
    arrange(desc(n))
}, rownames = FALSE, options = list(pageLength = 20))
