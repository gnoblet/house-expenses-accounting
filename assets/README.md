# Assets Folder

This folder contains default configuration files for the House Expense Calculator app.

## Files

- **people_list_default.txt**: Default list of people names (one per line)
- **expense_types_default.txt**: Default expense types (one per line)  
- **shared_expense_types_default.txt**: Default shared expense types that are always split equally (one per line)
- **exceptions_default.csv**: Default expense participation exceptions (CSV format)

## Usage

When the app starts, it will automatically load defaults from these files if they exist. If a file is missing or cannot be read, the app will use built-in fallback defaults.

### Text Files (.txt)
The text files are loaded into the app's text input areas as default values.

### Exceptions File (.csv)
The exceptions file is automatically loaded as default data when the app starts, unless you upload a different exceptions file.

## Customization

To customize the defaults for your household:

1. Edit the `.txt` and `.csv` files in this folder
2. Add or remove names/types as needed (one per line for .txt files)
3. For exceptions: use CSV format with Person,Type,Percentage columns
4. Save the files
5. Restart the app

The changes will be reflected in the default values shown in the app.

## Examples

**people_list_default.txt:**
```
Alice
Bob
Charlie
Diana
```

**expense_types_default.txt:**
```
Groceries
Utilities
Entertainment
Travel
```

**shared_expense_types_default.txt:**
```
Rent
Internet
Insurance
```

**exceptions_default.csv:**
```
Person,Type,Percentage
Bob,Alcohol,0.2
Alice,Party,0.5
Charlie,Alcohol,0.0
```

This means:
- Bob pays 20% of alcohol expenses (instead of full share)
- Alice pays 50% of party expenses  
- Charlie pays 0% of alcohol expenses (doesn't drink)
