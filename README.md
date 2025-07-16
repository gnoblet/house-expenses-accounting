# house-expenses-accounting

A Shiny app to split and track shared house expenses fairly among housemates. Upload your expenses and (optionally) absences/exceptions as CSV files, set your period, and get a clear summary of who owes what. Features weighted calculations for absences, expense exclusions, Excel export, and tabbed analysis views.

## Why

Traditional expense splitting apps treat everyone equally, but real life isn't that simple. This app handles:

- **Absences**: Someone away for a week shouldn't pay for groceries they didn't eat
- **Partial participation**: Bob drinks less alcohol, so he pays 20% instead of full share
- **Shared services**: Utilities and subscriptions are always split equally, regardless of absences
- **Transparency**: See exactly how calculations work, filter by expense type
- **Flexibility**: Upload your data, set custom rules, export professional reports
- **Fairness**: Non-manual calculations ensures everyone pays their true fair share

## Features

- **Smart calculations**: Weighted splits based on presence and exceptions
- **Shared expenses**: Some expense types always split equally (utilities, subscriptions)
- **Expense exclusions**: People can participate at reduced rates in specific expense types (e.g., Bob pays 20% of alcohol)
- **Dynamic date ranges**: Auto-adjusts to uploaded expense data
- **PDF export**: Download all results as formatted pdf
- **Tabbed interface**: Final settlement, summaries, details, and filtered analysis
- **Robust validation**: Clear error messages and warnings

## Quick Start

1. Open the app in R (see `app.R`).
2. Upload your expenses CSV (see `examples/expenses_example.csv`).
3. (Optional) Upload absences and/or exceptions CSV files.
4. Enter people and expense types (or use example files).
5. Set your date range and click **Calculate Expenses**.
6. View results in tabs, filter by expense type, download Excel report.

See the `examples/README.md` for file format details and tips.

## Assets - Default Configuration

The app supports customizable default values through the `assets/` folder. This makes it easy to set up consistent defaults for your household without modifying the code.

### Asset Files

- **`assets/people_list_default.txt`**: Default list of people names (one per line)
- **`assets/expense_types_default.txt`**: Default expense types (one per line)  
- **`assets/shared_expense_types_default.txt`**: Default shared expense types that are always split equally (one per line)
- **`assets/exceptions_default.csv`**: Default expense participation exceptions (CSV format)

### How It Works

When the app starts, it automatically:
1. Checks if asset files exist in the `assets/` folder
2. Loads content from text files into the app's input areas if they exist
3. Loads the exceptions CSV file as default data if it exists
4. Falls back to built-in defaults if files are missing or can't be read

### Customization

To customize defaults for your household:

1. **Edit the files**: Modify the `.txt` files in the `assets/` folder
2. **One entry per line**: Add or remove names/types as needed
3. **Save and restart**: Restart the app to see your changes
4. **Version control**: The `assets/` folder can be committed to git for team sharing

### Example Setup

**assets/people_list_default.txt:**
```
Alice
Bob
Charlie
Diana
Eve
Frank
Grace
Henry
```

**assets/expense_types_default.txt:**
```
Normal
Party
Alcohol
Special
```

**assets/shared_expense_types_default.txt:**
```
Utilities
Subscriptions
Insurance
Internet
```

**assets/exceptions_default.csv:**
```
Person,Type,Percentage
Bob,Alcohol,0.2
Alice,Party,0.5
```

This exceptions file means:
- Bob pays 20% of alcohol expenses (instead of full share)  
- Alice pays 50% of party expenses
- Everyone else pays their normal calculated share

### Benefits

- **Persistent**: Your customizations survive app restarts
- **Shareable**: Team members get the same defaults
- **Safe**: Built-in fallbacks if files are missing
- **No coding**: Just edit text files to customize
- **Flexible**: Different setups for different households

---

## Golem Framework

This app is built using the [Golem](https://engineering-shiny.org/golem.html) framework for production-grade Shiny applications. Golem enforces modularity, best practices, and a robust development workflow.

---

## How to Run Locally (Development)

**Do not run `app.R` directly.**

To launch the app in development mode, use the Golem workflow:

1. Open RStudio or your R console in the project root.
2. Run:

```r
source("dev/run_dev.R")
```

This will:
- Set development options
- Detach and reload all packages
- Document the package
- Start the app with all assets and modules loaded

---

## Project Folder Structure (Key Parts)

```
inst/app/www/assets/
    people_list_default.txt
    expense_types_default.txt
    shared_expense_types_default.txt
    exceptions_default.csv
    README.md
inst/app/www/examples/
    expenses_example.csv
    absences_example.csv
    exceptions_example.csv
    people_list_example.txt
    expense_types_example.txt
    shared_expense_types_example.txt
    README.md
```

- **assets/**: Default configuration files loaded at startup
- **examples/**: Example files for user reference and testing

---
