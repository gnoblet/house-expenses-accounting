#' Generate PDF Report
#'
#' @param calculations Calculations result
#' @param start_date Start date
#' @param end_date End date
#' @param absences_data Absences data
#' @param exceptions_data Exceptions data
#' @param file Output file path
#'
#' @export
generate_pdf_report <- function(
  calculations,
  start_date,
  end_date,
  absences_data,
  exceptions_data,
  file
) {
  message("Creating PDF report with R Markdown...")

  # Prepare data
  final_settlement <- calculations$final_settlement
  summary_by_type <- calculations$summary_by_type
  expenses <- calculations$expenses
  detailed_calculations <- calculations$detailed_calculations

  absences_summary <- if (!is.null(absences_data) && nrow(absences_data) > 0) {
    absences_data
  } else {
    data.frame(Person = character(0), Absent_Days = numeric(0))
  }

  exceptions_summary <- if (
    !is.null(exceptions_data) && nrow(exceptions_data) > 0
  ) {
    exceptions_data
  } else {
    data.frame(
      Person = character(0),
      Type = character(0),
      Percentage = numeric(0)
    )
  }

  # Create temporary R Markdown file
  temp_rmd <- tempfile(fileext = ".Rmd")

  # Generate R Markdown content
  rmd_content <- create_rmd_content(
    start_date,
    end_date,
    expenses,
    absences_summary,
    exceptions_summary,
    final_settlement,
    summary_by_type,
    detailed_calculations
  )

  # Write R Markdown content to temporary file
  writeLines(rmd_content, temp_rmd)

  # Render R Markdown document
  result <- tryCatch(
    {
      rmarkdown::render(
        input = temp_rmd,
        output_file = file,
        output_format = "pdf_document",
        quiet = FALSE
      )
      message("✓ PDF report created successfully with R Markdown")
      shiny::showNotification(
        "📄 PDF report generated successfully!",
        type = "message"
      )
      TRUE
    },
    error = function(e) {
      message("❌ R Markdown PDF creation failed: ", e$message)
      shiny::showNotification(
        paste("❌ Error creating PDF report:", e$message),
        type = "error"
      )
      # Create a simple CSV file as fallback
      tryCatch(
        {
          utils::write.csv(
            calculations$final_settlement,
            file,
            row.names = FALSE
          )
          shiny::showNotification(
            "⚠️ PDF creation failed - provided CSV file instead",
            type = "warning"
          )
        },
        error = function(e2) {
          message("❌ CSV fallback also failed: ", e2$message)
          writeLines("Error: Could not create report file", file)
        }
      )
      FALSE
    }
  )

  # Clean up temporary file
  if (file.exists(temp_rmd)) {
    file.remove(temp_rmd)
  }
}

#' Create R Markdown Content for PDF Report
#'
#' @param start_date Start date
#' @param end_date End date
#' @param expenses Expenses data frame
#' @param absences_summary Absences summary data frame
#' @param exceptions_summary Exceptions summary data frame
#' @param final_settlement Final settlement data frame
#' @param summary_by_type Summary by type data frame
#' @param detailed_calculations Detailed calculations data frame
#'
#' @return Character string with R Markdown content
create_rmd_content <- function(
  start_date,
  end_date,
  expenses,
  absences_summary,
  exceptions_summary,
  final_settlement,
  summary_by_type,
  detailed_calculations
) {
  paste0(
    '---
title: "House Expenses Report"
date: "',
    format(Sys.Date(), "%B %d, %Y"),
    '"
output: 
  pdf_document:
    latex_engine: xelatex
    number_sections: true
geometry: margin=0.8in
mainfont: Carlito
header-includes:
  - \\usepackage{booktabs}
  - \\usepackage{longtable}
  - \\usepackage{array}
  - \\usepackage{multirow}
  - \\usepackage{wrapfig}
  - \\usepackage{float}
  - \\usepackage{colortbl}
  - \\usepackage{pdflscape}
  - \\usepackage{tabu}
  - \\usepackage{threeparttable}
  - \\usepackage{threeparttablex}
  - \\usepackage[normalem]{ulem}
  - \\usepackage{makecell}
  - \\usepackage{xcolor}
  - \\definecolor{softgreen}{RGB}{144,190,109}
  - \\definecolor{softred}{RGB}{235,151,78}
params:
  start_date: "',
    start_date,
    '"
  end_date: "',
    end_date,
    '"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(knitr)
library(kableExtra)
library(dplyr)
library(scales)
```

# Executive Summary

**Report Period:** ',
    format(start_date, "%B %d, %Y"),
    ' to ',
    format(end_date, "%B %d, %Y"),
    '

**Total Expenses:** CHF ',
    sprintf("%.2f", sum(expenses$Amount, na.rm = TRUE)),
    '

**Number of Transactions:** ',
    nrow(expenses),
    '

**People Involved:** ',
    length(unique(expenses$Person)),
    '

',
    create_absences_section(absences_summary),
    '

',
    create_exceptions_section(exceptions_summary),
    '

',
    create_final_settlement_section(final_settlement),
    '

',
    create_summary_by_type_section(summary_by_type),
    '

',
    create_expense_details_section(expenses),
    '

',
    create_detailed_calculations_section(detailed_calculations),
    '
'
  )
}

#' Create Absences Section for RMD
#'
#' @param absences_summary Absences summary data frame
#'
#' @return Character string with absences section
create_absences_section <- function(absences_summary) {
  if (nrow(absences_summary) > 0) {
    paste0(
      '## Absences Summary

```{r absences-table}
absences_data <- data.frame(
  Person = c("',
      paste(absences_summary$Person, collapse = '", "'),
      '"),
  Absent_Days = c(',
      paste(absences_summary$Absent_Days, collapse = ', '),
      ')
)

kable(absences_data, 
      col.names = c("Person", "Absent Days"),
      caption = "Absence Summary") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                latex_options = c("hold_position"))
```'
    )
  } else {
    "## Absences Summary\n\nNo absences recorded for this period."
  }
}

#' Create Exceptions Section for RMD
#'
#' @param exceptions_summary Exceptions summary data frame
#'
#' @return Character string with exceptions section
create_exceptions_section <- function(exceptions_summary) {
  if (nrow(exceptions_summary) > 0) {
    paste0(
      '## Exceptions Summary

```{r exceptions-table}
exceptions_data <- data.frame(
  Person = c("',
      paste(exceptions_summary$Person, collapse = '", "'),
      '"),
  Type = c("',
      paste(exceptions_summary$Type, collapse = '", "'),
      '"),
  Percentage = c(',
      paste(exceptions_summary$Percentage, collapse = ', '),
      ')
)

exceptions_data$Percentage_Text <- paste0(round(exceptions_data$Percentage * 100, 1), "%")

kable(exceptions_data[,c("Person", "Type", "Percentage_Text")], 
      col.names = c("Person", "Expense Type", "Participation %"),
      caption = "Expense Participation Exceptions") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                latex_options = c("hold_position"))
```'
    )
  } else {
    "## Exceptions Summary\n\nNo exceptions applied for this period."
  }
}

#' Create Final Settlement Section for RMD
#'
#' @param final_settlement Final settlement data frame
#'
#' @return Character string with final settlement section
create_final_settlement_section <- function(final_settlement) {
  paste0(
    '# Final Settlement

```{r final-settlement}
final_data <- data.frame(
  Person = c("',
    paste(final_settlement$Person, collapse = '", "'),
    '"),
  Total_Paid = c(',
    paste(final_settlement$Total_Paid, collapse = ', '),
    '),
  Total_Owed = c(',
    paste(final_settlement$Total_Owed, collapse = ', '),
    '),
  Final_Balance = c(',
    paste(final_settlement$Final_Balance, collapse = ', '),
    ')
)

final_data <- final_data %>%
  arrange(desc(Final_Balance)) %>%
  mutate(
    Total_Paid_Text = paste0("CHF ", sprintf("%.2f", Total_Paid)),
    Total_Owed_Text = paste0("CHF ", sprintf("%.2f", Total_Owed)),
    Final_Balance_Text = paste0("CHF ", sprintf("%.2f", Final_Balance))
  )

kable(final_data[,c("Person", "Total_Paid_Text", "Total_Owed_Text", "Final_Balance_Text")], 
      col.names = c("Person", "Total Paid", "Total Owed", "Final Balance"),
      caption = "Final Settlement - Who Owes What") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                latex_options = c("hold_position")) %>%
  column_spec(4, color = ifelse(final_data$Final_Balance >= 0, "softgreen", "softred"), bold = TRUE)
```'
  )
}

#' Create Summary by Type Section for RMD
#'
#' @param summary_by_type Summary by type data frame
#'
#' @return Character string with summary by type section
create_summary_by_type_section <- function(summary_by_type) {
  paste0(
    '# Summary by Expense Type

```{r summary-by-type}
summary_data <- data.frame(
  Type = c("',
    paste(summary_by_type$Type, collapse = '", "'),
    '"),
  Total_Amount = c(',
    paste(summary_by_type$Total_Amount, collapse = ', '),
    '),
  Total_Paid = c(',
    paste(summary_by_type$Total_Paid, collapse = ', '),
    '),
  Total_Owed = c(',
    paste(summary_by_type$Total_Owed, collapse = ', '),
    ')
)

summary_data <- summary_data %>%
  mutate(
    Total_Amount_Text = paste0("CHF ", sprintf("%.2f", Total_Amount)),
    Total_Paid_Text = paste0("CHF ", sprintf("%.2f", Total_Paid)),
    Total_Owed_Text = paste0("CHF ", sprintf("%.2f", Total_Owed))
  )

kable(summary_data[,c("Type", "Total_Amount_Text", "Total_Paid_Text", "Total_Owed_Text")], 
      col.names = c("Expense Type", "Total Amount", "Total Paid", "Total Owed"),
      caption = "Summary by Expense Type") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                latex_options = c("hold_position"))
```'
  )
}

#' Create Expense Details Section for RMD
#'
#' @param expenses Expenses data frame
#'
#' @return Character string with expense details section
create_expense_details_section <- function(expenses) {
  paste0(
    '# Expense Details

```{r expense-details}
expense_data <- data.frame(
  Date = as.Date(c("',
    paste(expenses$Date, collapse = '", "'),
    '")),
  Type = c("',
    paste(expenses$Type, collapse = '", "'),
    '"),
  Person = c("',
    paste(expenses$Person, collapse = '", "'),
    '"),
  Reason = c("',
    paste(gsub('"', '\\\\\\"', expenses$Reason), collapse = '", "'),
    '"),
  Amount = c(',
    paste(expenses$Amount, collapse = ', '),
    ')
)

expense_data <- expense_data %>%
  arrange(desc(Date)) %>%
  mutate(
    Date_Text = format(Date, "%Y-%m-%d"),
    Amount_Text = paste0("CHF ", sprintf("%.2f", Amount))
  )

kable(expense_data[,c("Date_Text", "Type", "Person", "Reason", "Amount_Text")], 
      col.names = c("Date", "Type", "Person", "Reason", "Amount"),
      caption = "All Expense Details") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                latex_options = c("hold_position", "scale_down"))
```'
  )
}

#' Create Detailed Calculations Section for RMD
#'
#' @param detailed_calculations Detailed calculations data frame
#'
#' @return Character string with detailed calculations section
create_detailed_calculations_section <- function(detailed_calculations) {
  paste0(
    '# Detailed Calculations

```{r detailed-calculations}
detailed_data <- data.frame(
  Person = c("',
    paste(detailed_calculations$Person, collapse = '", "'),
    '"),
  Type = c("',
    paste(detailed_calculations$Type, collapse = '", "'),
    '"),
  Total_Paid = c(',
    paste(detailed_calculations$Total_Paid, collapse = ', '),
    '),
  Share_Owed = c(',
    paste(detailed_calculations$Share_Owed, collapse = ', '),
    '),
  Balance = c(',
    paste(detailed_calculations$Balance, collapse = ', '),
    ')
)

detailed_data <- detailed_data %>%
  filter(Total_Paid > 0 | Share_Owed > 0) %>%
  arrange(Type, desc(Balance)) %>%
  mutate(
    Total_Paid_Text = paste0("CHF ", sprintf("%.2f", Total_Paid)),
    Share_Owed_Text = paste0("CHF ", sprintf("%.2f", Share_Owed)),
    Balance_Text = paste0("CHF ", sprintf("%.2f", Balance))
  )

kable(detailed_data[,c("Person", "Type", "Total_Paid_Text", "Share_Owed_Text", "Balance_Text")], 
      col.names = c("Person", "Type", "Paid", "Owed", "Balance"),
      caption = "Detailed Calculations by Person and Type") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), 
                latex_options = c("hold_position", "scale_down")) %>%
  column_spec(5, color = ifelse(detailed_data$Balance >= 0, "softgreen", "softred"), bold = TRUE)
```'
  )
}
