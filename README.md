# house-expenses-accounting

A Shiny app to split and track shared house expenses fairly among housemates. Upload your expenses and (optionally) absences/exceptions as CSV files, set your period, and get a clear summary of who owes what. Features weighted calculations for absences, expense exclusions, Excel export, and tabbed analysis views.

## Why

Traditional expense splitting apps treat everyone equally, but real life isn't that simple. This app handles:

- **Absences**: Someone away for a week shouldn't pay for groceries they didn't eat
- **Partial participation**: Bob drinks less alcohol, so he pays 20% instead of full share
- **Shared services**: Utilities and subscriptions are always split equally, regardless of absences
- **Transparency**: See exactly how calculations work, filter by expense type
- **Flexibility**: Upload your data, set custom rules, export professional reports
- **Fairness**: Mathematical precision ensures everyone pays their true fair share

## Features

- **Smart calculations**: Weighted splits based on presence and exceptions
- **Shared expenses**: Some expense types always split equally (utilities, subscriptions)
- **Expense exclusions**: People can participate at reduced rates in specific expense types (e.g., Bob pays 20% of alcohol)
- **Dynamic date ranges**: Auto-adjusts to uploaded expense data
- **Excel export**: Download all results as formatted spreadsheet
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