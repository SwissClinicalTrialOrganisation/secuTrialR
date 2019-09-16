# Readme for sT_exports
This readme file contains information about file structure and naming conventions used in the directory inst/extdata/sT_exports.

## Directory structure

- **BMD** - contains secuTrial exports associated with the Bone Mineral Density (BMD) dataset
- **encodings** - contains secuTrial exports relevant for encoding
- **longnames** - contains a collection of secuTrial exports with long table names
- **shortnames** - contains a collection of secuTrial exports with short table names

## File naming

Follow a naming convention is used for all secuTrial exports contained within inst/extdata/sT_exports:

- **s_export_CSV-xls** - all exports start with this string, followed by underscore separated tags listed below
- **project tag** - short alpha numeric tag in capital letters that stands for the secuTrial database the data was extracted from
- **rt** - export in rectangular table format
- **short** - export with shortened table names
- **long** - export with full table names
- **meta** - export with duplicated form metadata in all tables
- **ref** - export with reference values in a separate table
- **no-id** - export with without Add-ID and without Pat-ID
- **miss** - export with missing values
- **language tag** - export in a language. possible tags: en / de / fr / es / it / pl / unsupported
- **encoding tag** - export encoding settings e.g. "utf16", "utf8", etc.
