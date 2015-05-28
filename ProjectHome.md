## DORM is an ORM for Delphi ##

### Features ###
  * Open Source _Apache License 2.0_
  * Database agnostic (Do not require database changes!)
  * **Has one**, **has many** and **belongs to** relations support
  * Mapping through file, attributes or CoC
  * Save and retrieve _objects graph_, not only single objects
  * External (file) or internal (resource, json stream) configuration
  * Interfaces based!
  * Supports for
    * FirebirdSQL (using UIB)
    * Interbase (using UIB)
    * SQLServer (using FireDAC driver)
    * SQLite3 (using [this](http://www.ararat.cz/doku.php/en:sqlitewrap) SQLite3 wrapper)
  * Event based validation (OnBeforeInsert, ObAfterInsert, OnBeforeUpdate, OnAfterUpdate and so on)
  * Persistence ignorance
  * Object Versioning
  * Object Tracking
  * Can persists everythig!
  * Used for years in big (hundred of tables with complex logic) 3tier systems and in many other smaller systems
  * Very good performances
  * Completely Unit tested
  * Multiple environments
    * Development
    * Test
    * Production
  * Lazy Load for related objects
  * Rudimental support for null (currently only in the FIREBIRD persistence adapter)
  * Fluent interfaces for queries
  * Unit Of Work for multiple operations
  * Use anon methods and generics


Tested on Delphi XE7, XE6, XE5, XE4, XE3 and XE2 (Win32)


Little introduction to dorm is available [here](http://delphi-orm.googlecode.com/files/dorm_introduction.pdf) as PDF