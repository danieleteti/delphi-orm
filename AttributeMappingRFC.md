# Attributes Mapping Examples #

Let's say that there isn't ANY kind of file mapping.
By convention:
The classe is mapped for a table of same name without **"T"** prefix.
Each property is mapped for a column with same name. The property types will be mapped for compatible database column types, default sizes and scales.
The first property with name **Id** property will be considered the table's primary key.
Lets I want to use the default mapping behaviour for my TPerson class:

### Sample 1 ###
```
[Entity]
TPerson = class
public
  property id: integer;
  property Name: String...
  property Address: String...
  property Age: Integer...
end;
```
This class will be directly mapped to the following table structure:

|TABLE NAME|PERSON|
|:---------|:-----|
|Columns   |Type/Size/PK|
|ID        |INTEGER (PK)|
|NAME      |VARCHAR with default size|
|ADDRESS   |VARCHAR with default size|
|AGE       |NUMBER|

### Sample 2 ###
```
[Entity('PEOPLE')]
TPerson = class
public
  property id: integer;
  property Name: String...
...
```
On this case we have the Person class but in database we need map this class to table named **PEOPLE**.

### Sample 3 ###
```
[Entity('PEOPLE')]
TPerson = class
public
  property id: integer;
  [Column('FULL_NAME', 60)]
  property Name: String...
  property Address: String...
  property Age: Integer...
end;
```
On this case the table PEOPLE don't the **Name** column, but have **FULL\_NAME** column with VARCHAR(60).

### Sample 4 ###
```
[Entity]
TPerson = class
public
  [Id]
  property Identifier: integer;
  property Name: String...
  property Address: String...
  property Age: Integer...
end;
```
On this case, we have a property called **Identifier** that will be persisted to a column with same name, but this column is too the table's primary key column. We need annotate this property with _[Id](Id.md)_ to Dorm knows that this field is the primary key column.

### Sample 5 ###
```
[Entity]
TPerson = class
public
  [Id]
  [Column('Identifier')]
  property MyOtherIdentifier: integer;
  property Name: String...
  property Address: String...
  property Age: Integer...
end;
```
Look that we can too put more that one annotation for same property. In this case my table column is called **Identifier** and is primary key.

### Sample 6 ###
```
[Entity]
TCar = class
public
  [Id('dorm.adapter.Firebird.TMyOwnerTableSequence')]
  property Id: integer;
  ...
end;
```
On Dorm, Each database strategy has a default generator class strategy.
The default class to generate primary key to Firebird databases is **dorm.adapter.Firebird.TFirebirdTableSequence**
When is necessary to use a different strategy we can map like above sample. **TMyOwnerTableSequence** will be used when Dorm need create a new sequence to Id field on Car table.

![http://cdn1.iconfinder.com/data/icons/gnome-desktop-icons-png/PNG/32/Gnome-Dialog-Question-32.png](http://cdn1.iconfinder.com/data/icons/gnome-desktop-icons-png/PNG/32/Gnome-Dialog-Question-32.png) We need define what the default strategy for each database or driver.

### Sample 7 ###
```
[Entity]
TPerson = class
private
  FAge: Integer;
  ...
  function GetIsAdult: boolean;
public
  property id: integer;
  property Name: String...
  property Address: String...
  property Age: Integer...
  [Transient]
  property IsAdult: boolean read GetIsAdult;
end;

function TPerson.GetIsAdult;
begin
  Result := FAge > 18;
end;
```
Now we have a field called **"IsAdult"** that don't have a corresponding field on table **Person**). This is called transient property. To Dorm don't put this field on sql'a statments we can use this annotation [Transient](Transient.md). This is very used when the class have calculated properties.

### Sample 8 ###
```
[Entity]
TPerson = class
public
  property id: integer;
  property Name: String...
  property Address: String...
  property Age: Integer...
  [HasMany(TPhone)]
  property Phones: TObjectList<TPhone>...
end;

[Entity('PHONES')]
TPhone = class
protected
  property PersonOID: integer...
public
  property id: integer...
  property Number: String...
end;
```
On this new case we have relation between two tables. This relation is many _**one to many**_ (1:n). Dorm map this relation using the attribute **[HasMany](HasMany.md)** so a person can have one or more phones.
Look that with this kind of relation the table **PHONES** have a id (_**PersonOID**_) property that point to owner (some person). The property PersonOID is mapped as foreign key on TPhone class by convention without need map.

### Sample 9 ###
```
[Entity]
TPerson = class
public
  property id: integer;
  property Name: String...
  property Address: String...
  property Age: Integer...
  [HasMany(TPhone, 'OwnerID')]
  property Phones: TObjectList<TPhone>...
end;

[Entity('PHONES')]
TPhone = class
protected
  [Column('PARENT_PERSON_ID')]
  property OwnerID: integer...
public
  property id: integer...
  property Number: String...
end;
```
When we have a specific property name on many side (TPhone) we can define a second parameter on HasMany attribute point to it (OwnerOD). Look too that on many side the property can be mapped for a field with another name (PARENT\_PERSON\_ID).

Please, add other mapping examples here... we've to share the ideas about the attributes mapping.

![http://cdn1.iconfinder.com/data/icons/gnome-desktop-icons-png/PNG/32/Gnome-Dialog-Question-32.png](http://cdn1.iconfinder.com/data/icons/gnome-desktop-icons-png/PNG/32/Gnome-Dialog-Question-32.png) Need sample:
  * Inheritance
  * ManyToOne
  * ManyToMany
  * Cascade operations
  * Inverse relations