# house-expenses-accounting

A Shiny app to split and track shared house expenses fairly among housemates. Upload your expenses and (optionally) absences/exceptions as CSV files, set your period, and get a clear summary of who owes what. Features weighted calculations for absences, expense exclusions, Excel export, and tabbed analysis views.

## Features

- **Smart calculations**: Weighted splits based on presence and exceptions
- **Expense exclusions**: People can opt out of specific expense types (e.g., alcohol)
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