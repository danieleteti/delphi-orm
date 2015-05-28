  * New PersistentStrategy to allow use Firebird with Delphi Professional
  * Many to many relations
  * Inherited´s relations
  * Automatic optimistic locking through object versioning
  * Support for strongly typed collections - **DONE**
```
TPerson = class(Tobject)
  ...
  Phones: TObjectList<TPhone>;
  ...
end;
```
  * Differents Mapping Strategies (Custom, File, Attributes, Convention) - **Branch/Testing**
  * Per-Object custom mappers
  * More databases support
    * MySQL
    * Oracle
    * SQLite - **DONE**
    * MSSQLServer - **Branch/Testing**
  * File based (not SQL) supports
    * JSON
    * CSV
    * XML
  * Split mapping config and persistence config in 2 separate files
  * More tools
    * Create config file from existing databases
    * Create classes from config file
  * Demos, demos, demos
  * Build a strong community