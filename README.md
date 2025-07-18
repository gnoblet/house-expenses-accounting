# house-expenses-accounting

Welcome to **house-expenses-accounting** – the smarter, fairer way to split and track shared house expenses! Whether you’re living with friends, family, or housemates, this Shiny app helps you handle the real-life messiness of shared costs with clarity, flexibility, and transparency.

<div align="center">
  <img src="inst/app/www/screenshots/house-expenses-accounting_homepage.png" alt="App Homepage" width="600"/>
  <br/>
  <em>Get started with an intuitive homepage and clear workflow</em>
</div>

---

## Why Choose This App?

Often, expense splitters treat everyone equally, but **house-expenses-accounting** is designed for the realities of shared living:

- **Absences**: Someone away for a week shouldn’t pay for groceries they didn’t eat.
- **Partial participation**: Bob drinks less alcohol, so he pays 20% instead of a full share.
- **Shared services**: Utilities and subscriptions are always split equally, regardless of absences.
- **Transparency**: See exactly how calculations work, filter by expense type.
- **Flexibility**: Upload your data, set custom rules, export professional reports.
- **Fairness**: Automated calculations ensure everyone pays their true fair share.

---

## Features at a Glance

- **Smart calculations**: Weighted splits based on presence and exceptions.
- **Shared expenses**: Some expense types always split equally (utilities, subscriptions).
- **Expense exclusions**: People can participate at reduced rates in specific expense types (e.g., Bob pays 20% of alcohol).
- **Dynamic date ranges**: Auto-adjusts to uploaded expense data.
- **PDF export**: Download all results as a formatted PDF.
- **Tabbed interface**: Final settlement, summaries, details, and filtered analysis.
- **Robust validation**: Clear error messages and warnings.

<div align="center">
  <img src="inst/app/www/screenshots/house-expenses-accounting_results-type.png" alt="Results by Type" width="600"/>
  <br/>
  <em>Analyze expenses by type and see exactly how each cost is split</em>
</div>

---

## See It in Action

### 1. Upload Your Data

- Upload your expenses CSV (see `examples/expenses_example.csv`).
- (Optional) Upload absences and/or exceptions CSV files.
- Enter people and expense types (or use example files).

### 2. Set Your Period and Calculate

- Set your date range and click **Calculate Expenses**.
- The app dynamically adjusts to your data and rules.

### 3. Explore Results

- View results in multiple tabs: settlement, summaries, details, and filtered analysis.
- Filter by expense type, see who owes what, and download a professional Excel or PDF report.

<div align="center">
  <img src="inst/app/www/screenshots/house-expenses-accounting_settlement.png" alt="Settlement Tab" width="600"/>
  <br/>
  <em>Final settlement tab: see exactly who owes what, and why</em>
</div>

---

## Advanced Analysis

Want to see how your household’s spending changes over time? The app includes longitudinal and trend analysis features to help you spot patterns and optimize your shared budget.

<div align="center">
  <img src="inst/app/www/screenshots/house-expenses-accounting_trendanalysis.png" alt="Trend Analysis" width="600"/>
  <br/>
  <em>Spot trends and get insights with built-in analysis tools</em>
</div>

---

## Quick Start

1. Open the app in R (see `app.R`).
2. Upload your expenses CSV.
3. (Optional) Upload absences and/or exceptions CSV files.
4. Enter people and expense types (or use example files).
5. Set your date range and click **Calculate Expenses**.
6. Explore results, filter by expense type, and download reports.

See the `examples/README.md` for file format details and tips.

---

## Customizing Your Defaults

The app supports customizable default values through the `assets/` folder. This makes it easy to set up consistent defaults for your household—no coding required!

### Asset Files

- **`assets/people_list_default.txt`**: Default list of people names (one per line)
- **`assets/expense_types_default.txt`**: Default expense types (one per line)
- **`assets/shared_expense_types_default.txt`**: Default shared expense types that are always split equally (one per line)
- **`assets/exceptions_default.csv`**: Default expense participation exceptions (CSV format)

### How It Works

When the app starts, it automatically:
1. Checks if asset files exist in the `assets/` folder.
2. Loads content from text files into the app’s input areas if they exist.
3. Loads the exceptions CSV file as default data if it exists.
4. Falls back to built-in defaults if files are missing or can’t be read.

### Customization Steps

1. **Edit the files**: Modify the `.txt` files in the `assets/` folder.
2. **One entry per line**: Add or remove names/types as needed.
3. **Save and restart**: Restart the app to see your changes.
4. **Version control**: The `assets/` folder can be committed to git for team sharing.

#### Example Setup

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

**Benefits:**
- **Persistent**: Your customizations survive app restarts.
- **Shareable**: Team members get the same defaults.
- **Safe**: Built-in fallbacks if files are missing.
- **No coding**: Just edit text files to customize.
- **Flexible**: Different setups for different households.

---

## Built with Golem

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

## Next Steps

- [ ] Add tests and checks
- [ ] Provide longitudinal analysis (and other features?)
- [ ] Review design and viz approach

---

Ready to make your house finances fair, transparent, and drama-free?
**Try house-expenses-accounting today!**
