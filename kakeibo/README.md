# Kakeibo

Kakeibo is intended to be a personal expense manager program that tracks expense usage over time.

## Disclaimer

This is a personal project done for educational purposes of learning haskell
programming and building haskell code that runs on multiple devices and
environments. As such it is NOT intended to be a complete product to be used by
a consumer. There are many more useful expense trackers out there that are full
featured products if you are looking for something that is more practical to
use in real life.

## Main features

- Multi-channel expense reporting: web interface, desktop client, CLI, mobile device
- Expense collection by category
- Email summaries
- Savings tracking

## Folder/App Structure

- Kakeibo
    - Server
        - Models
        - Views
        - Controllers
        - Static
        - Commands
        - Utils
        - Middlewares
    - CLI
    - Desktop
    - Web
    - Mobile

## Models

- User
    - userId :: UserId
    - username :: Text
    - password :: ByteString
    - email :: Text
    - dateJoined :: UTCTime
- MoneyTransfer
    - transferId :: MoneyTransferId
    - userId :: UserId
    - amount :: Float
    - created :: UTCTime
    - description :: Text
    - category :: Text
- BankState
    - bankStateId :: BankStateId
    - userId :: UserId
    - amount :: Float
    - bankName :: Text
    - timestamp :: UTCTime

## Design

Kakeibo will track every user reported money transfer, both income and
expenses, which can be grouped by the category to track spending in certain
areas. The reported money transfer can then be applied to the amounts in the
bank to determine how much money the user has left in their accounts. However,
since everything is user reported, this can become out of sync. As such, the
user also has the ability to adjust the bank state by directly entering the
bank amounts. This is then reported as a discrepancy, which the user can choose
to correct by adding unaccounted for expenses and income.

A positive amount on MoneyTransfer is income vs. a negative amount which is an expense.

## Needed interfaces

- Way for user to enter expenses and incomes
- Way for user to enter bank state
- Way for user to see expense usage report

## Technology stack

- Scotty for web server
- Fay for web client
- Acid State for persistent storage

## Implemented commands

server-command:

- `server-command user add` Adds a new user to the system
- `server-command user list` Lists all users on the server

cli:

- `cli list` List all transfers for user
- `cli add` Add a new transfer for user
- Assumes API token is set on KAKEIBO_API_TOKEN env var

## TODO

- Start developing frontend web client
   - Account management page
   - MoneyTransfer management page
