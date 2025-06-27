# House Expense Calculator - Example Files

This folder contains example files to help you understand the expected format for each input in the House Expense Calculator.

## 📁 Files Included

### 1. `expenses_example.csv` 
**Format:** CSV file with exactly 5 columns in this order:
- **Type**: Category of expense (must match your expense types list)
- **Reason**: Description of the expense
- **Date**: Date in DD/MM/YYYY format (e.g., 15/01/2025)
- **Amount**: Numeric amount (e.g., 85.50)
- **Person**: Name of person who paid (must match your people list)

### 2. `absences_example.csv` (Optional)
**Format:** CSV file with exactly 2 columns:
- **Person**: Name of person (must match your people list)
- **Absent_Days**: Number of days absent during the period (0 or positive integer)

### 3. `people_list_example.txt`
**Format:** Plain text file with one name per line
- Each person who participates in shared expenses
- Names must exactly match those used in expenses and absences files

### 4. `expense_types_example.txt`
**Format:** Plain text file with one expense type per line
- Categories used to group expenses
- Types must exactly match those used in expenses file

### 5. `shared_expense_types_example.txt`
**Format:** Plain text file with one expense type per line
- Expense types that are always split equally among all people
- Ignores absences, exceptions, and presence ratios (e.g., utilities, subscriptions)
- Types must exactly match those used in expenses file

### 6. `exceptions_example.csv` (Optional)
**Format:** CSV file with exactly 3 columns:
- **Person**: Name of person (must match your people list)
- **Type**: Expense type they participate in at reduced rate (must match your expense types)
- **Percentage**: Participation rate (0.0 = no participation, 0.5 = half participation, etc.)
- **Purpose**: Allows certain people to pay reduced shares of specific expense categories (e.g., Bob pays 20% of alcohol since he drinks less)

## 💡 Tips

1. **Case Sensitivity**: Names and expense types are case-sensitive
2. **Date Format**: Always use DD/MM/YYYY format for dates
3. **Numbers**: Use decimal point (.) for amounts, not comma (,)
4. **Consistency**: Make sure all names and types match exactly across files
5. **Exceptions**: Use exceptions file to set reduced participation rates for specific expense types (0.0-0.99)
6. **Shared types**: Use shared expense types for equal splits regardless of absences/exceptions
7. **Absences**: If you don't have an absences file, everyone is assumed present for all days

## 🚨 Common Validation Errors

- **👥 People mismatch**: Person in expenses not in people list
- **🏷️ Type mismatch**: Expense type in expenses not in types list  
- **❌ Missing people**: Person in people list missing from absences file
- **⚠️ Invalid days**: Negative absent days or days exceeding period length

## 📊 How It Works

1. Upload your expenses CSV (required)
2. Optionally upload absences CSV and/or exceptions CSV
3. Set your date range for calculation
4. Enter people names and expense types (or copy from example files)
5. Click "Calculate Expenses" to see results
