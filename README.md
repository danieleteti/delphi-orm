#DORM, the "Delphi ORM"
Delphi ORM (DORM for short) is a powerful ORM for Delphi. It is quite robust and tested because used since 2010 in production for many business critical systems.

##Features
* Open Source Apache License 2.0
* Database agnostic (Do not require database changes!)
* Has one, has many and belongs to relations support
* Mapping through file, attributes or CoC
* Save and retrieve objects graph, not only single objects
* External (file) or internal (resource, json stream) configuration
* Interfaces based!
* Supports for
 * FirebirdSQL (using UIB, ZeosDBO)
 * Interbase (using UIB)
 * SQLServer (using FireDAC driver)
 * SQLite3 (using this SQLite3 wrapper)
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
* Rudimental support for null (currently only in the FIREBIRD and in the MSSQLServer persistence adapter)
* Fluent interfaces for queries
* Unit Of Work for multiple operations
* Use anon methods and generics
* Tested on Delphi 10Seattle, XE8, XE7, XE6, XE5, XE4, XE3 and XE2 (Win32)

Little introduction to dorm is available [here as PDF](https://github.com/danieleteti/delphi-orm/blob/master/docs/Introduction%20to%20DORM.pdf).
To understand how dorm works, please check the `HelloWorldSamples.groupproj` project group into the folder `Samples/<YOUR DELPHI VERSION>`. 
